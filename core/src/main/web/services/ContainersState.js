app.service("ContainersState", ["Api", function(Api) {
	var me = this;

	me.state = Api.subscribeSnapshot("easyrider.Infrastructure$ContainerState", function(snapshot) {
	});

	var unknown = { jsonClass: "unknown" };

	me.containerStatus = function(containerId) {
	    if (me.state.loading) {
	        return unknown;
	    }
        var status = me.state.snapshot.entries[containerId.stageId.applicationId.id + ":" + containerId.stageId.id + ":" + containerId.id];
        if (status) {
            return status;
        } else {
		    return unknown;
		}
	};
}]);
