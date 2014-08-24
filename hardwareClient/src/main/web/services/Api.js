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
	};

	function forceAuthenticated() {
		if(!me.isAuthenticated)
			throw new Error("You must be authenticated first");
	}

	var subscriptionsRequested = [];
	var sendAfterAuthentication = [];

	function Subscription(subscriptionId) {
		this.subscriptionId = subscriptionId;
		this.subscribed = false;
		this.snapshot = {};
	}

	var subscriptions = {};

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
		if(me.isAuthenticated)
			Connection.send(subscription);
		subscriptionsRequested.push(subscription);
		var s = new Subscription(subscriptionId);
		subscriptions[subscriptionId] = s;
		s.eventKey = subscription.eventKey;
		s.eventType = subscription.eventType;
		s.commandId = subscription.commandId;
		return s;
	};

	me.command = function(jsonClass, command) {
		command.jsonClass = jsonClass;
		command.commandId = { id: nextId() };
		if(me.isAuthenticated)
			Connection.send(command);
		else
			sendAfterAuthentication.push(command);
	};

	Connection.on["easyrider.Api$Authentication"] = function() {
		me.isAuthenticated = true;
		for(var i in subscriptionsRequested)
			Connection.send(subscriptionsRequested[i]);
		for(i in sendAfterAuthentication)
			Connection.send(sendAfterAuthentication[i]);
		sendAfterAuthentication = [];
	};

	Connection.on["easyrider.Events$Subscribed"] = function(msg) {
		var s = subscriptions[msg.subscriptionId];
		s.subscribed = true;
		s.snapshot = msg.snapshot;
	};
}]);