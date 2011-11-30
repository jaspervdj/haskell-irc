function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':8282' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

function appendMessage(message) {
    $('#chat').append(message + '\n');
}

function Tab(tabManager, name, channel) {
    var tab = this;
    var active = false;

    var title = $(document.createElement('div')).text(name);
    title.hide();
    $('#tab-titles').append(title);

    var button = $(document.createElement('div')).text(name);
    $('#tab-buttons').append(button);

    var panel = $(document.createElement('div')).addClass('tab');
    panel.hide();
    $('#tab-panels').append(panel);

    button.click(function() {
        tabManager.showTab(tab);
    });

    this.hide = function() {
        panel.hide();
        title.hide();
        active = false;
    }

    this.show = function() {
        panel.show();
        title.show();
        active = true;
        button.removeClass('alert');
    };

    this.appendMessage = function(message) {
        var div = $(document.createElement('div'));
        div.text(message);
        panel.append(div);
        if(!active) button.addClass('alert');
    };

    this.getChannel = function() {
        return channel;
    };

    this.setTitle = function(text) {
        title.text(text);
    };
}

function TabManager() {
    var tabManager = this;
    var serverTab = new Tab(tabManager, 'server');
    var activeTab = serverTab;

    var channelTabs = {};
    
    this.showTab = function(tab) {
        activeTab.hide();
        tab.show();
        activeTab = tab;
    };

    this.getServerTab = function() {
        return serverTab;
    };

    this.getActiveTab = function() {
        return activeTab;
    };

    this.getChannelTab = function(channel) {
        if(!channelTabs[channel]) {
            channelTabs[channel] = new Tab(tabManager, channel, channel);
        }
        return channelTabs[channel];
    };
}

function makeHandlers(tabManager, nick) {
    return {
        "privmsg": function(event) {
            var channel = event.channel;
            var tab = tabManager.getChannelTab(channel);
            tab.appendMessage(event.nick + ': ' + event.text);
        },
        "join": function(event) {
            /* We joined a channel, open a new tab */
            if(nick == event.nick) {
                var tab = tabManager.getChannelTab(event.channel);
                tabManager.showTab(tab);
            }
        },
        "topic": function(event) {
            var tab = tabManager.getChannelTab(event.channel);
            tab.setTitle(event.text);
        }
    };
}

function connect(server, port, nick) {
    var ws = createWebSocket('/chat');
    $('#connect').hide();
    $('#chat').show();

    var tabManager = new TabManager();
    var handlers = makeHandlers(tabManager, nick);
    var serverTab = tabManager.getServerTab();

    ws.onmessage = function(event) {
        var json;
        try {
            json = JSON.parse(event.data);
        } catch(err) {
            json = null;
        }

        if(json) {
            if(handlers[json.type]) {
                handlers[json.type](json);
            } else {
                serverTab.appendMessage('[No handler] ' + JSON.stringify(json));
            }
        } else {
            serverTab.appendMessage('[No JSON] ' + event.data);
        }
    };

    ws.onopen = function() {
        ws.send(JSON.stringify({
            "type": "connect",
            "server": server,
            "port": port,
            "nick": nick
        }));

        $('#join-form').submit(function() {
            var channel = $('#join-channel').val();
            ws.send(JSON.stringify({
                "type": "join",
                "channel": channel,
                "nick": nick
            }));
            return false;
        });

        $('#send-form').submit(function() {
            var channel = tabManager.getActiveTab().getChannel();
            var text = $('#send-text').val();
            if(channel) {
                ws.send(JSON.stringify({
                    "type": "privmsg",
                    "channel": channel,
                    "nick": nick,
                    "text": text
                }));
                $('#send-text').val('');
            }
            return false;
        });
    };

    ws.onerror = function(event) {
        appendMessage(event);
    };

    ws.onclose = function() {
        appendMessage('Closed');
    };
}

$(document).ready(function() {
    $('#connect-form').submit(function() {
        var server = $('#connect-server').val();
        var port = Number(server.replace(/.*:/, ''));
        server = server.replace(/:.*/, '');
        var nick = $('#connect-nick').val();
        connect(server, port, nick);
        return false;
    });
});
