function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':8282' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

function connect(server, port, nick) {
    var ws = createWebSocket('/chat');
    $('#connect').hide();
    ws.onmessage = function(event) {
        $('#chat').append(event.data);
    }
    ws.onopen = function() {
        ws.send(JSON.stringify({
            "type": "connect",
            "server": server,
            "port": port,
            "nick": nick
        }));
    }
    ws.onerror = function(event) {
        $('#chat').append(event);
    };
    ws.onclose = function() {
        $('#chat').append('Closed');
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
