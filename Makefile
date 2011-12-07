COFFEE=`npm bin`/coffee

client.js: client.coffee
	${COFFEE} -c client.coffee
