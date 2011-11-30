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

function makeHandlers() {
    return {
        "privmsg": function(event) {
            appendMessage('[' + event.channel + '] ' +
                    event.nick + ': ' + event.text);
        }
    };
}

function connect(server, port, nick) {
    var ws = createWebSocket('/chat');
    $('#connect').hide();

    var handlers = makeHandlers();

    ws.onmessage = function(event) {
        try {
            json = JSON.parse(event.data);
            if(handlers[json.type]) {
                handlers[json.type](json);
            } else {
                appendMessage('[No handler] ' + JSON.stringify(json));
            }
        } catch(err) {
            appendMessage('[No JSON] ' + event.data);
        }
    }

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
