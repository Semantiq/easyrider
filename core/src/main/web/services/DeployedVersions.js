app.service("DeployedVersions", ["Api", function(Api) {
	var me = this;

	me.deployment = Api.subscribeSnapshot("easyrider.Infrastructure$DeploymentInfo", function() {
	});

    var emptyList = [];
	me.deployedVersionsByContainerId = function(containerId) {
		var lst = [];
        var prefix = containerId.stageId.applicationId.id + ":" + containerId.stageId.id + ":" + containerId.id + ":";
        if (!me.deployment.snapshot) {
            return lst;
        }

        angular.forEach(me.deployment.snapshot.entries, function(value, key) {
			if (key.lastIndexOf(prefix, 0) === 0) {
			    lst.push(value);
			}
        });

		if(lst.length === 0) {
		    return emptyList;
		}

		return lst;
	};
}]);
