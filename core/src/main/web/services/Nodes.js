app.service('Nodes', ['Api', function(Api) {
    var me = this;

    me.allIds = [];

	me.state = Api.subscribeSnapshot("easyrider.Infrastructure$NodeState", function(snapshot) {
        me.allIds.length = 0;
        angular.forEach(snapshot.entries, function(value, key) {
            me.allIds.push({
                jsonClass: "easyrider.NodeId",
                id: key
            });
        });
	});
}]);
