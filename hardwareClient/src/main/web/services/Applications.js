app.service('Applications', ['Api', "Validators", "Utils", function(Api, Validators, Utils) {
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

	Validators.addValidator("easyrider.Applications$CreateApplication",
		function(command, reporter) {
		if(!Utils.isValidId(command.application.id.id))
			reporter.fail("Invalid application id", Utils.idInfo);
	});
}]);