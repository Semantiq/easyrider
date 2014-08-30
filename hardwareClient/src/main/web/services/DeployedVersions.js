app.service("DeployedVersions", ["Api", function(Api) {
	var me = this;

	me.subscription = Api.subscribe("easyrider.Infrastructure$VersionDeploymentProgressEvent", []);
	me.list = me.subscription.snapshot;

    var emptyList = [];
	me.deployedVersionsByContainerId = function(containerId) {
		var lst = [];

		for(var i in me.list) {
			var eventKey = me.list[i].eventDetails.eventKey.key;

			if(eventKey[0] == containerId.stageId.applicationId.id &&
			    eventKey[1] == containerId.stageId.id &&
			    eventKey[2] == containerId.id) lst.push(me.list[i]);
		}

		if(lst.length === 0)
		    return emptyList;

		return lst;
	};
}]);
