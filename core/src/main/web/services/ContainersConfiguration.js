app.service("ContainersConfiguration", ["Api", "Validators", "Utils", function(Api, Validators, Utils) {
	var me = this;

	me.containers = Api.subscribeSnapshot("easyrider.Applications$ContainerConfiguration", function(snapshot) {
	});

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
	me.removeContainerTemplate = function(containerId) {
	    return {
	        jsonClass: 'easyrider.Infrastructure$RemoveContainer',
	        containerId: containerId,
	        force: false
	    };
	};

	me.containersInStage = function(stageId) {
	    if (!stageId) return [];
		var lst = [];
        if (!me.containers.snapshot) {
            return lst;
        }

        angular.forEach(me.containers.snapshot.entries, function(value, key) {
			if (stageId.id == value.id.stageId.id && stageId.applicationId.id == value.id.stageId.applicationId.id) {
				lst.push(value);
            }
        });

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