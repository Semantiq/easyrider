app.service('EventLog', ['Api', function(Api) {
  var me = this;
  me.subscription = Api.subscribe("easyrider.Applications$StageUpdatedEvent", []);
  me.events = me.subscription.tail;
}]);
