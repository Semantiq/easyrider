app.service('Applications', ['Api', function(Api) {
	var me = this;

	me.subscription = Api.subscribe("easyrider.Applications$ApplicationUpdatedEvent", []);
	me.list = me.subscription.snapshot;

	me.addApplicationTemplate = function() {
		return {
			jsonClass: "easyrider.Applications$CreateApplication",
			application: {
				id: {
					id: ""
				},
				properties: [ ]
			}
		};
	};
	me.removeApplicationTemplate = function(applicationId) {
		return {
			jsonClass: "easyrider.Applications$RemoveApplication",
			applicationId: applicationId
		};
	};
}]);