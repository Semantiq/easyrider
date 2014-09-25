app.service("Api", ["Connection", function(Connection) {
	var me = this;
	me.isAuthenticated = false;

	var indexId = 0;
	var idPrefix = "console";

	function nextId() {
		return idPrefix + indexId++;
	}

	me.objects = {
		Authenticate: function() {
			this.jsonClass = "easyrider.Api$AuthenticateUser";
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
		this.snapshot = [];
	}

	var subscriptions = {};

	me.subscribe = function(eventType, eventKey) {
	    if(!Connection.on[eventType])
	        defineEvent(eventType);
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
				name: eventType,
				sender: {
					id: "core",
					jsonClass: "easyrider.ComponentId"
				}
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
		while(s.snapshot.length > 0) {
			s.snapshot.pop();
		}
		for(var i in msg.snapshot) {
			s.snapshot.push(msg.snapshot[i]);
		}
	};
	function handleFailure(msg) {
		alert(msg.message);
	}
	Connection.on["easyrider.Failure"] = handleFailure;
	Connection.on["easyrider.business.http.WebServerWorker$MessageFormatError"] = handleFailure;

	function defineEvent(className) {
		Connection.on[className] = function(event) {
			var eventKey = event.eventDetails.eventKey.key.join("::");
			for(var i in subscriptions) {
				var s = subscriptions[i];
				if(s.eventType.name == className) {
					var any = false;
					for(var j in s.snapshot) {
						if(s.snapshot[j].eventDetails.eventKey.key.join("::") == eventKey) {
							if(event.eventDetails.removal) {
								s.snapshot.splice(j, 1);
							} else {
								s.snapshot[j] = event;
							}
							any = true;
							break;
						}
					}
					if(!any && !event.eventDetails.removal) {
						s.snapshot.push(event);
					}
					if (!s.tail) {
					    s.tail = [];
					}
					s.tail.push(event);
					alert("tail: " + s.tail);
				}
			}
		};
	}
}]);