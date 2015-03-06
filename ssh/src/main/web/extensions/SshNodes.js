app.service('SshNodes', ['Api', 'Validators', 'Utils', function(Api, Validators, Utils) {
	var me = this;

	me.configuration = Api.subscribeSnapshot("easyrider.NodeConfiguration", function(config) {});
	me.nodeStates = Api.subscribeSnapshot("easyrider.Infrastructure$NodeState", function(state) {});

	me.addSshNodeTemplate = function() {
		return {
			jsonClass: "easyrider.Nodes$CreateNode",
			nodeConfiguration: {
				id: {
					id: ""
				},
				nodeType: "ssh",
                properties: [
                    {
                        "jsonClass":"easyrider.Property",
                        "namespace":"ssh",
                        "name":"host",
                        "value":""
                    },
                    {
                        "jsonClass":"easyrider.Property",
                        "namespace":"ssh",
                        "name":"port",
                        "value":"22"
                    },
                    {
                        "jsonClass":"easyrider.Property",
                        "namespace":"ssh",
                        "name":"login",
                        "value":""
                    },
                    {
                        "jsonClass":"easyrider.Property",
                        "namespace":"ssh",
                        "name":"password",
                        "value":""
                    }
                ]
			}
		};
	};

	Validators.addValidator("easyrider.Nodes$CreateNode",
		function(command, reporter) {
		if(!Utils.isValidId(command.nodeConfiguration.id.id))
			reporter.fail("Invalid node id", Utils.idInfo);
	});
}]);
