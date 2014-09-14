var http = require('http');
http.createServer(function (req, res) {
	res.writeHead(200, {'Content-Type': 'text/plain'});
	res.end('EasyRider test application says hello');
}).listen(8081, '127.0.0.1');
