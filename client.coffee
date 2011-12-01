createWebSocket = (path) ->
  host = window.location.hostname
  host = 'localhost' if(host == '')
  uri = 'ws://' + host + ':8282' + path

  if window.MozWebSocket
    new MozWebSocket(uri)
  else
    new WebSocket(uri)

class Tab
  constructor: (@tabManager, @name, @channel) ->
    @tab = tab = this
    @active = false
    @namesList = []

    @title = $(document.createElement('div')).text(@name)
    @title.hide()
    $('#tab-titles').append(@title)

    @button = $(document.createElement('div')).text(@name)
    $('#tab-buttons').append(@button)

    @panel = $(document.createElement('div')).addClass('panel')
    @panel.hide()
    $('#tab-panels').append(@panel)

    @names = $(document.createElement('div')).addClass('names')
    @names.hide()
    $('#tab-names').append(@names)

    @button.click(() ->
      tabManager.showTab(tab)
    )

  hide: () ->
    @panel.hide()
    @title.hide()
    @names.hide()
    @active = false

  show: () ->
    @panel.show()
    @title.show()
    @names.show()
    @active = true
    @button.removeClass('alert')

  appendMessage: (message) ->
    div = $(document.createElement('div'))
    div.text(message)
    @panel.append(div)
    @button.addClass('alert') if(!@active)

  getChannel: () ->
    @channel

  setTitle: (text) ->
    @title.text(text)

  refreshNames: () ->
    tab = this
    @names.html('')

    for name in @namesList
      do (name) ->
        div = $(document.createElement('div')).text(name.prefix + name.nick)
        div.click(() ->
          nameTab = tab.tabManager.getChannelTab(name.nick)
          tab.tabManager.showTab(nameTab)
        )
        tab.names.append(div)

  addNames: (names) ->
    @namesList.push(name) for name in names
    @tab.refreshNames()

class TabManager
  constructor: () ->
    @tabManager = this
    @serverTab = new Tab(@tabManager, 'server')
    @activeTab = @serverTab
    @channelTabs = {}
    
  showTab: (tab) ->
    @activeTab.hide()
    tab.show()
    @activeTab = tab

  getServerTab: () ->
    @serverTab

  getActiveTab: () ->
    @activeTab

  getChannelTab: (channel) ->
    unless @channelTabs[channel]
      @channelTabs[channel] = new Tab(@tabManager, channel, channel)
    @channelTabs[channel]

makeHandlers = (tabManager, nick) ->
  privmsg: (event) ->
    channel = event.channel
    tab =
      if event.channel == nick
        tabManager.getChannelTab(event.nick)
      else
        tabManager.getChannelTab(channel)
    tab.appendMessage(event.nick + ': ' + event.text)

  join: (event) ->
    # We joined a channel, open a new tab
    if nick == event.nick
      tab = tabManager.getChannelTab(event.channel)
      tabManager.showTab(tab)

  topic: (event) ->
    tab = tabManager.getChannelTab(event.channel)
    tab.setTitle(event.text)

  names: (event) ->
    tab = tabManager.getChannelTab(event.channel)
    tab.addNames(event.names)

connect = (server, port, nick) ->
  ws = createWebSocket('/chat')
  $('#connect').hide()
  $('#chat').show()

  tabManager = new TabManager()
  handlers = makeHandlers(tabManager, nick)
  serverTab = tabManager.getServerTab()

  ws.onmessage = (event) ->
    json = null
    try
      json = JSON.parse(event.data)
    catch err
      json = null

    if json
      if handlers[json.type]
        handlers[json.type](json)
      else
        serverTab.appendMessage('[No handler] ' + JSON.stringify(json))
    else
      serverTab.appendMessage('[No JSON] ' + event.data)

  ws.onopen = () ->
    ws.send(JSON.stringify({
      "type": "connect",
      "server": server,
      "port": port,
      "nick": nick
    }))

    $('#join-form').submit(() ->
      channel = $('#join-channel').val()
      ws.send(JSON.stringify({
        "type": "join",
        "channel": channel,
        "nick": nick
      }))
      false
    )

    $('#send-form').submit(() ->
      channel = tabManager.getActiveTab().getChannel()
      text = $('#send-text').val()
      if channel
        ws.send(JSON.stringify({
          "type": "privmsg",
          "channel": channel,
          "nick": nick,
          "text": text
        }))
        $('#send-text').val('')
      false
    )

$(document).ready(() ->
  $('#connect-form').submit(() ->
    try
      server = $('#connect-server').val()
      port = Number(server.replace(/.*:/, ''))
      server = server.replace(/:.*/, '')
      nick = $('#connect-nick').val()
      connect(server, port, nick)
    catch err
      alert(err + ' at ' + err.lineNumber)
    false
  )
)
