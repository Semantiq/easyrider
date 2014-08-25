app.service("ContainersConfiguration", ["Api", "Validators", "Utils", function(Api, Validators, Utils) {
	var me = this;

	me.subscription = Api.subscribe("easyrider.Applications$ContainerConfigurationUpdatedEvent", []);
	me.list = me.subscription.snapshot;

	me.addContainerConfigurationTemplate = function(stageId) {
		return {
			jsonClass: "easyrider.Applications$CreateContainerConfiguration",
			container: {
				id: {
					stageId: stageId,
					id: ""
				},
				properties: [ ]
			}
		};
	};

	me.containersInStage = function(stageId) {
		var lst = [];

		for(var i in me.list) {
			var cce = me.list[i];

			// FIXME
			lst.push(cce);
		}

		return lst;
	};

	Validators.addValidator("easyrider.Applications$CreateContainerConfiguration",
		function(command, reporter) {
		if(!Utils.isValidId(command.container.id.id))
			reporter.fail("Invalid container id", Utils.idInfo);
	});
}]);