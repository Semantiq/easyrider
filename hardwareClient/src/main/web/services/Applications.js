app.service('Applications', ['Api', function(Api) {
	var me = this;

	me.subscription = Api.subscribe("easyrider.Applications$ApplicationUpdatedEvent", "");
	me.list = me.subscription.snapshot;

	me.addApplication = function() {
		
	};
}]);