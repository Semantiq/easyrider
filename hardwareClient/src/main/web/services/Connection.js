app.service("Connection", ["$rootScope", function($rootScope) {
	var me = this;
	me.open = false;
	me.on = {};

	me.lastCloudMessage = 0;
	me.lastToCloudMessage = 0;

	function onMessage(msg) {
		$rootScope.$apply(function() {
			if(me.on[msg.jsonClass])
				me.on[msg.jsonClass](msg);
			else {
				console.error("Unhandled message ", msg);
			}
		});
	}

	function sendToCloud() {
		me.lastToCloudMessage = new Date().getTime();
		setTimeout(function() {
			$rootScope.$apply(function() { });
		}, 500);
	}

	var pending = [];

	me.send = function(msg) {
		var json = JSON.stringify(msg);
		if(me.open) {
			me.ws.send(json);
			sendToCloud();
		} else
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
			sendToCloud();
			$rootScope.$apply(me.onOpen);
		};
		me.ws.onmessage = function(e) {
			me.lastCloudMessage = new Date().getTime();
			onMessage(JSON.parse(e.data));
			setTimeout(function() {
				$rootScope.$apply(function() { });
			}, 700);
		};
	}

	connect();
}]);

app.controller("ConnectionInfoCtrl", ["$scope", "Connection", function($scope, Connection) {
	$scope.toUser = function() {
		return { opacity: (new Date().getTime() - Connection.lastCloudMessage < 600) ? 1 : 0.2 };
	};
	$scope.toCloud = function() {
		return { opacity: (new Date().getTime() - Connection.lastToCloudMessage < 400) ? 1 : 0.2 };
	};
}]);