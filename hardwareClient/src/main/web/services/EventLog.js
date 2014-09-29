app.service('EventLog', ['Api', function(Api) {
  var me = this;
  me.subscription = Api.subscribe("easyrider.Infrastructure$ContainerStateChangedEvent", []);
  me.events = me.subscription.tail;
}]);
