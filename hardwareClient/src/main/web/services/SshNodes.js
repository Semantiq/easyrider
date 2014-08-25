app.service('SshNodes', ['Api', "Validators", "Utils", function(Api, Validators, Utils) {
	var me = this;

	me.subscription = Api.subscribe("easyrider.SshInfrastructure$NodeConfigurationUpdated", []);
	me.list = me.subscription.snapshot;

	me.addSshNodeTemplate = function() {
		return {
			jsonClass: "easyrider.SshInfrastructure$CreateNode",
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
//	me.removeApplicationTemplate = function(applicationId) {
//		return {
//			jsonClass: "easyrider.Applications$RemoveApplication",
//			applicationId: applicationId
//		};
//	};
//
	Validators.addValidator("easyrider.SshInfrastructure$CreateNode",
		function(command, reporter) {
		if(!Utils.isValidId(command.nodeConfiguration.id.id))
			reporter.fail("Invalid node id", Utils.idInfo);
	});
}]);