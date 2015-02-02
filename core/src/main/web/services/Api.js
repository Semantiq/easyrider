app.service("Api", ["Connection", "$interval", function(Connection, $interval) {
	var me = this;
	me.isAuthenticated = false;

	var indexId = 0;
	var idPrefix = "console";

	function nextId() {
		return idPrefix + indexId++;
	}

	me.objects = {
		Authenticate: function(username, password) {
			this.jsonClass = "easyrider.Api$AuthenticateUser";
			this.username = username;
			this.password = password;
		}
	};

	me.authenticate = function(auth) {
		Connection.send(auth);
		me.authObject = auth;
	};

	Connection.onOpen = function() {
		if(me.authObject) {
			Connection.send(me.authObject);
		}
	};

	function forceAuthenticated() {
		if(!me.isAuthenticated)
			throw new Error("You must be authenticated first");
	}

	var subscriptionsRequested = [];
	var replaysRequested = [];
	var sendAfterAuthentication = [];
	var snapshots = {};

	function Subscription(subscriptionId, callback) {
		this.subscriptionId = subscriptionId;
		this.subscribed = false;
		this.snapshot = [];
		this.callback = callback;
	}

	var subscriptions = {};

    me.subscribeSnapshot = function(entryType, callback) {
        if (!Connection.onSnapshotUpdate[entryType]) {
            defineEntry(entryType);
        }
        var startSubscription = {
            jsonClass: "easyrider.Events$StartSnapshotSubscription",
            commandDetails: makeCommandDetails(),
            entryType: {
                jsonClass: "easyrider.SnapshotEntryType",
                clazz: entryType
            }
        };
        snapshots[entryType] = {
            loading: true,
            callback: callback
        };
        if (me.isAuthenticated) {
            Connection.send(startSubscription);
        } else {
            sendAfterAuthentication.push(startSubscription);
        }
        return snapshots[entryType];
    };

	me.subscribe = function(eventType, eventKey, callback) {
	    if(!Connection.on[eventType]) {
	        defineEvent(eventType);
	    }
	    // TODO: this will not prevent duplicated subscriptions if they are nested
		var subscriptionId = eventType + "::" + eventKey.join("::");
        if (subscriptions[subscriptionId]) {
            return subscriptions[subscriptionId];
        }
		var subscription = {
			jsonClass: "easyrider.Events$Subscribe",
			commandDetails: makeCommandDetails(),
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
		var replayQueryId = nextId();
		var replay = {
            jsonClass: "easyrider.Events$GetReplay",
            queryId: {
                jsonClass: "easyrider.QueryId",
                id: replayQueryId
            },
            subscriptions: [ subscriptionId ],
            since: "2000-01-01T00:00:00.000Z"
		};
		if(me.isAuthenticated) {
		    replaysRequested[replayQueryId] = subscription;
		    //console.log("replaysRequested: " + angular.toJson(replaysRequested));
			Connection.send(subscription);
			Connection.send(replay);
		}
		subscriptionsRequested.push(subscription);
		var s = new Subscription(subscriptionId, callback);
		subscriptions[subscriptionId] = s;
		s.eventKey = subscription.eventKey;
		s.eventType = subscription.eventType;
		s.commandId = subscription.commandId;
		return s;
	};

	me.command = function(jsonClass, command) {
		command.jsonClass = jsonClass;
		command.commandDetails = {
            jsonClass: "easyrider.CommandDetails",
		    commandId: {
		        jsonClass: "easyrider.CommandId",
		        id: nextId()
		    },
		    traceMode: {
		        jsonClass: "easyrider.TraceMode"
		    }
		};
		if(me.isAuthenticated)
			Connection.send(command);
		else
			sendAfterAuthentication.push(command);
	};

	Connection.on["easyrider.Api$Authentication"] = function() {
		me.isAuthenticated = true;
        me.isAuthenticationFailure = false;
		for(var i in subscriptionsRequested)
			Connection.send(subscriptionsRequested[i]);
		for(i in sendAfterAuthentication)
			Connection.send(sendAfterAuthentication[i]);
		sendAfterAuthentication = [];
		Connection.keepAlive = $interval(function() {
			Connection.send({jsonClass: 'easyrider.Api$KeepAlive'});
		}, 30000);
		// TODO: cancel on disconnect
	};

	Connection.on["easyrider.Api$AuthenticationFailure"] = function() {
		me.isAuthenticated = false;
        me.isAuthenticationFailure = true;
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
		if (s.callback) {
		    s.callback();
		}
	};
	Connection.on["easyrider.Events$GetReplayResponse"] = function(msg) {
        var s = replaysRequested[msg.queryId.id];
        //console.log(angular.toJson(replaysRequested) + ": " + angular.toJson(s.tail) + " <- " + angular.toJson(msg));
        s.tail = msg.events;
        delete replaysRequested[msg.queryId.id];
	};
	function handleFailure(msg) {
		alert(msg.message);
	}
	Connection.on["easyrider.Commands.Failure"] = handleFailure;
	Connection.on["easyrider.business.http.WebServerWorker$MessageFormatError"] = handleFailure;
    Connection.on["easyrider.Events$SnapshotSubscriptionStarted"] = function(message) {
        var snapshot = snapshots[message.snapshot.entryType.clazz];
        snapshot.snapshot = message.snapshot;
        snapshot.loading = false;
        snapshot.callback(snapshot.snapshot);
    };
    Connection.on["easyrider.Events$SnapshotUpdatedEvent"] = function(message) {
        Connection.onSnapshotUpdate[message.update.entryType](message.update);
    };

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
					if (s.callback) {
					    s.callback();
					}
				}
			}
		};
	}

	function defineEntry(className) {
        Connection.onSnapshotUpdate[className] = function(update) {
            var snapshot = snapshots[update.entryType.jsonClass];
            if (update.entry) {
                snapshot.snapshot.entries[update.eventKey] = update.entry;
            } else {
                delete snapshot.snapshot.entries[update.eventKey];
            }
            snapshot.callback(snapshot.snapshot);
        };
	}

	function makeCommandDetails() {
	    return {
            jsonClass: "easyrider.CommandDetails",
            commandId: {
                jsonClass: "easyrider.CommandId",
                id: nextId()
            },
            traceMode: {
                jsonClass: "easyrider.TraceMode"
            }
        };
	}
}]);
