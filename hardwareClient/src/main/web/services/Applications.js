app.service('Applications', ['Api', function(Api) {
	var me = this;

	me.subscription = Api.subscribe("easyrider.Applications$ApplicationUpdatedEvent", []);
	me.list = me.subscription.snapshot;

	me.addApplication = function(app) {
		console.log(Api);
		Api.command("easyrider.Applications$CreateApplication", {
			application: app
		});
	};

	me.Application = function(id, properties) {
		this.id = { id: id };
		this.properties = properties;
	};
}]);