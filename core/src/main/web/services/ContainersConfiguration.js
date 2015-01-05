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
	me.deployVersionTemplate = function(containerId) {
		return {
			jsonClass: "easyrider.Infrastructure$DeployVersion",
			containerId: containerId,
			version: { }
		};
	};
	me.unDeployVersionTemplate = function(containerId, version) {
		return {
			jsonClass: "easyrider.Infrastructure$UnDeployVersion",
			containerId: containerId,
			version: version
		};
	};
	me.startContainerTemplate = function(containerId, version) {
	    return {
	        jsonClass: 'easyrider.Infrastructure$StartContainer',
	        containerId: containerId,
            version: version
	    };
	};
	me.stopContainerTemplate = function(containerId) {
	    return {
	        jsonClass: 'easyrider.Infrastructure$StopContainer',
	        containerId: containerId
	    };
	};

	me.containersInStage = function(stageId) {
	    if (!stageId) return [];
		var lst = [];

		for(var i in me.list) {
			var cce = me.list[i];

			if(stageId.id == cce.container.id.stageId.id && stageId.applicationId.id == cce.container.id.stageId.applicationId.id)
				lst.push(cce);
		}

		return lst;
	};
    me.updateContainerConfigurationTemplate = function(container) {
        return {
            jsonClass: "easyrider.Applications$UpdateContainerConfiguration",
            container: container.container
        };
    };

	Validators.addValidator("easyrider.Applications$CreateContainerConfiguration",
		function(command, reporter) {
		if(!Utils.isValidId(command.container.id.id))
			reporter.fail("Invalid container id", Utils.idInfo);
	});
}]);