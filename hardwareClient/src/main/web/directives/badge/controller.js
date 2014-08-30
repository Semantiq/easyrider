app.directive("badge", function() {
	return {
		restrict: "E",
		scope: {
			state: "=",
			label: "="
		},
		template: "<span style='font-size:13.5px;line-height:27px' class='label label-{{labelColor[StatusClasses[state.jsonClass].state]}}'><i class='{{icon[StatusClasses[state.jsonClass].state]}}'></i>&nbsp;{{label||StatusClasses[state.jsonClass].label}}</span>",
		controller: ["$scope", "StatusClasses", function($scope, StatusClasses) {
		    $scope.StatusClasses = StatusClasses;

			$scope.icon = {
				unknown: "fa fa-question",
				pending: "fa fa-circle-o-notch fa-spin",
				success: "fa fa-check",
				failure: "fa fa-close",
				paused: "fa fa-pause"
			};
			$scope.labelColor = {
				unknown: "default",
				pending: "warning",
				success: "success",
				failure: "danger",
				paused: "default"
			};
		}]
	};
});