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

function Tab(name) {
    var tab = this;

    var button = $(document.createElement('div')).text(name);
    $('#tab-buttons').append(button);
    this.button = button;

    var panel = $(document.createElement('div'));
    panel.hide();
    $('#tab-panels').append(panel);
    this.panel = panel;

    this.button.click(function() {
        tab.show();
    });

    this.show = function() {
        $('#tab-panels').children('div').hide();
        panel.show();
    };

    this.appendMessage = function(message) {
        var div = $(document.createElement('div'));
        div.text(message);
        panel.append(div);
    };
}

function makeHandlers() {
    mainTab = new Tab('main');
    tabs = {};

    return {
        "privmsg": function(event) {
            var channel = event.channel;
            if(!tabs[channel]) tabs[channel] = new Tab(channel);
            tabs[channel].appendMessage(event.nick + ': ' + event.text);
        }
    };
}

function connect(server, port, nick) {
    var ws = createWebSocket('/chat');
    $('#connect').hide();

    var handlers = makeHandlers();

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
                appendMessage('[No handler] ' + JSON.stringify(json));
            }
        } else {
            appendMessage('[No JSON] ' + event.data);
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
                "channel": channel
            }));
            return false;
        });
    }
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
