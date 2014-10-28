app.service('SshNodes', ['Api', "Validators", "Utils", function(Api, Validators, Utils) {
	var me = this;

	me.subscription = Api.subscribe("easyrider.business.ssh.SshInfrastructure$NodeConfigurationUpdatedEvent", []);
	me.stateSubscription = Api.subscribe("easyrider.Infrastructure$NodeUpdatedEvent", []);
	me.list = me.subscription.snapshot;
	me.stateList = me.stateSubscription.snapshot;

	me.addSshNodeTemplate = function() {
		return {
			jsonClass: "easyrider.business.ssh.SshInfrastructure$CreateNode",
			nodeConfiguration: {
				id: {
					id: ""
				},
				host: "",
				port: 22,
				login: "",
				password: ""
			}
		};
	};

	me.nodeStates = function() {
        var nodes = {
        };
        for (var i in me.stateList) {
            nodes[me.stateList[i].eventDetails.eventKey.key[0]] = me.stateList[i];
        }
        return nodes;
	};
//	me.removeApplicationTemplate = function(applicationId) {
//		return {
//			jsonClass: "easyrider.Applications$RemoveApplication",
//			applicationId: applicationId
//		};
//	};
//
	Validators.addValidator("easyrider.business.ssh.SshInfrastructure$CreateNode",
		function(command, reporter) {
		if(!Utils.isValidId(command.nodeConfiguration.id.id))
			reporter.fail("Invalid node id", Utils.idInfo);
	});
}]);