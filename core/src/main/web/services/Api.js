app.service("Api", ["Connection", "$interval", "$timeout", "$cookieStore", function(Connection, $interval, $timeout, $cookieStore) {
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

    me.setAuthObject = function(authObject) {
        $cookieStore.put("authObject", authObject);
    };
	me.getAuthObject = function() {
	    return $cookieStore.get("authObject");
	};
    me.currentUser = function() {
        if (me.isAuthenticated) {
            return me.getAuthObject();
        }
    };
	me.authenticate = function(auth) {
		Connection.send(auth);
		me.setAuthObject(auth);
	};

	Connection.onOpen = function() {
	    var authObject = me.getAuthObject();
		if(authObject) {
			Connection.send(authObject);
		}
	};

	function forceAuthenticated() {
		if(!me.isAuthenticated)
			throw new Error("You must be authenticated first");
	}

	var subscriptionsRequested = [];
	var sendAfterAuthentication = [];
	var snapshots = {};
	me.runningCommands = {};

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
        if (!snapshots[entryType]) {
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
                callbacks: [ callback ]
            };
            if (me.isAuthenticated) {
                Connection.send(startSubscription);
            } else {
                sendAfterAuthentication.push(startSubscription);
            }
        } else {
            var snapshot = snapshots[entryType];
            snapshot.callbacks.push(callback);
            if (snapshot.snapshot) {
                callback(snapshot.snapshot);
            }
        }
        return snapshots[entryType];
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
		if(me.isAuthenticated) {
			Connection.send(command);
		} else {
			sendAfterAuthentication.push(command);
		}
		me.runningCommands[command.commandDetails.commandId.id] = {};
	};

    Connection.onExecution = function(message) {
        if (me.runningCommands[message.executionOf.id]) {
            me.runningCommands[message.executionOf.id] = message;
            if (message.successMessage) {
                $timeout(function() {
                    delete me.runningCommands[message.executionOf.id];
                }, 1000);
            }
            if (message.failureMessage) {
                $timeout(function() {
                    delete me.runningCommands[message.executionOf.id];
                }, 10000);
            }
        } else {
            console.log("Ignoring execution of " + message.executionOf.id + ": " + angular.toJson(message));
        }
    };

	Connection.on["easyrider.Api$Authentication"] = function(message) {
		me.isAuthenticated = true;
        me.isAuthenticationFailure = false;
        console.log("Authentication result: " + angular.toJson(message));
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

	function handleFailure(msg) {
		alert(msg.message);
	}
	Connection.on["easyrider.Commands.Failure"] = handleFailure;
	Connection.on["easyrider.business.http.WebServerWorker$MessageFormatError"] = handleFailure;
    Connection.on["easyrider.Events$SnapshotSubscriptionStarted"] = function(message) {
        var snapshot = snapshots[message.snapshot.entryType.clazz];
        snapshot.snapshot = message.snapshot;
        snapshot.loading = false;
        angular.forEach(snapshot.callbacks, function(callback) {
            callback(snapshot.snapshot);
        });
    };
    Connection.on["easyrider.Events$SnapshotUpdatedEvent"] = function(message) {
        Connection.onSnapshotUpdate[message.update.entryType.clazz](message.update);
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
            var snapshot = snapshots[update.entryType.clazz];
            if (update.entry) {
                snapshot.snapshot.entries[update.eventKey] = update.entry;
            } else {
                delete snapshot.snapshot.entries[update.eventKey];
            }
            angular.forEach(snapshot.callbacks, function(callback) {
                callback(snapshot.snapshot);
            });
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
