app.service("Command", ["Api", function(Api) {
	this.show = function(command) {
		this.command = command;
	};

	this.command = null;

	this.cancel = function() {
		this.command = null;
	};

	this.execute = function() {
		Api.command(this.command.jsonClass, this.command);
		this.cancel();
	};
}]);

app.controller("CommandCtrl", ["$scope", "Command", function($scope, Command) {
	$scope.Command = Command;
}]);