app.directive("eventLog", function() {
	return {
		restrict: "E",
		templateUrl: "/directives/eventLog/template.html",
		scope: {
			filter: "=",
			eventTypes: "="
		},
		controller: ["$scope", "EventLog", function($scope, EventLog) {
		    $scope.events = EventLog.subscription.tail;
		}]
	};
});
