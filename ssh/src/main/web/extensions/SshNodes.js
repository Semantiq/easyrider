app.service('SshNodes', ['Api', 'Validators', 'Utils', function(Api, Validators, Utils) {
	var me = this;

	me.configuration = Api.subscribeSnapshot("easyrider.business.ssh.SshInfrastructure$NodeConfiguration", function(config) {});
	me.nodeStates = Api.subscribeSnapshot("easyrider.Infrastructure$NodeState", function(state) {});

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

	Validators.addValidator("easyrider.business.ssh.SshInfrastructure$CreateNode",
		function(command, reporter) {
		if(!Utils.isValidId(command.nodeConfiguration.id.id))
			reporter.fail("Invalid node id", Utils.idInfo);
	});
}]);
