app.service("Api", ["Connection", function(Connection) {
	var me = this;
	me.isAuthenticated = false;

	var indexId = 0;
	var idPrefix = "cmd" + Math.random() + "seq";

	function nextId() {
		return idPrefix + indexId++;
	}

	me.objects = {
		Authenticate: function() {
			this.jsonClass = "easyrider.Api$Authenticate";
		}
	};

	me.authenticate = function(auth) {
		Connection.send(auth);
		me.authObject = auth;
	};

	Connection.onOpen = function() {
		if(me.authObject)
			Connection.send(me.authObject);
		for(var i in subscriptionsRequested)
			Connection.send(subscriptionsRequested[i]);
	}

	function forceAuthenticated() {
		if(!me.isAuthenticated)
			throw new Error("You must be authenticated first");
	}

	var subscriptionsRequested = [];

	me.subscribe = function(eventType, eventKey) {
		var subscriptionId = nextId();
		var subscription = {
			jsonClass: "easyrider.Events$Subscribe",
			commandId: {
				jsonClass: "easyrider.CommandId",
				id: nextId()
			},
			subscriptionId: subscriptionId,
			eventType: {
				jsonClass: "easyrider.EventType",
				name: eventType
			},
			eventKey: {
				jsonClass: "easyrider.EventKey",
				key: eventKey.push ? eventKey : eventKey.split(" ")
			}
		};
		Connection.send(subscription);
		subscriptionsRequested.push(subscription);
	};

	Connection.on["easyrider.Api$Authentication"] = function() {
		me.isAuthenticated = true;
	};
}]);