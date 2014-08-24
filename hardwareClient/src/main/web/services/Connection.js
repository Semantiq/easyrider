app.service("Connection", ["$rootScope", function($rootScope) {
	var me = this;
	me.open = false;
	me.on = {};

	function onMessage(msg) {
		$rootScope.$apply(function() {
			if(me.on[msg.jsonClass])
				me.on[msg.jsonClass](msg);
			else {
				console.error("Unhandled message ", msg);
			}
		});
	}

	var pending = [];

	me.send = function(msg) {
		var json = JSON.stringify(msg);
		if(me.open)
			me.ws.send(json);
		else
			pending.push(json);
	};

	function connect() {
		me.ws = new WebSocket("ws://" + location.host);
		me.ws.onclose = function() {
			me.open = false;
			setTimeout(function() {
				connect();
			}, 500);
		};
		me.ws.onopen = function() {
			me.open = true;
			for(var i in pending)
				me.ws.send(pending[i]);
			pending = [];
			$rootScope.$apply(me.onOpen);
		};
		me.ws.onmessage = function(e) {
			onMessage(JSON.parse(e.data));
		};
	}

	connect();
}]);