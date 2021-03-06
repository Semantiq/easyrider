app.service('Applications', ['Api', "Validators", "Utils", function(Api, Validators, Utils) {
	var me = this;

    me.ids = [];
	me.applications = Api.subscribeSnapshot("easyrider.Applications$Application", function(snapshot) {
        me.ids.length = 0;
        angular.forEach(snapshot.entries, function(value, key) {
            me.ids.push(value.id);
        });
	});

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
	me.updateApplicationTemplate = function(application) {
		return {
			jsonClass: "easyrider.Applications$UpdateApplication",
			application: application
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