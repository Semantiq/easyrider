app.service("ContainersState", ["Api", function(Api) {
	var me = this;

	me.subscription = Api.subscribe("easyrider.Infrastructure$ContainerStateChangedEvent", []);
	me.list = me.subscription.snapshot;

	var unknown = { jsonClass: "unknown" };

	me.containerStatus = function(containerId) {
		for(var i in me.list) {
			var status = me.list[i];
            var eventKey = status.eventDetails.eventKey.key;
			if(eventKey[0] == containerId.stageId.applicationId.id &&
			    eventKey[1] == containerId.stageId.id &&
			    eventKey[2] == containerId.id) return status.state;
		}
		return unknown;
	};
}]);
