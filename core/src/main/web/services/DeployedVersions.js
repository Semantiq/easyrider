app.service("DeployedVersions", ["Api", function(Api) {
	var me = this;

	me.subscription = Api.subscribe("easyrider.Infrastructure$VersionDeploymentProgressEvent", []);
	me.list = me.subscription.snapshot;

    var emptyList = [];
	me.deployedVersionsByContainerId = function(containerId) {
		var lst = [];
        var prefix = containerId.stageId.applicationId.id + ":" + containerId.stageId.id + ":" + containerId.id + ":";

		for(var i in me.list) {
			var eventKey = me.list[i].eventDetails.eventKey;
			if (eventKey.lastIndexOf(prefix, 0) === 0) {
			    lst.push(me.list[i]);
			}
		}

		if(lst.length === 0) {
		    return emptyList;
		}

		return lst;
	};
}]);
