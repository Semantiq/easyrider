app.service("Command", ["Api", "Validators", function(Api, Validators) {
	var me = this;

	me.show = function(command) {
		me.command = command;
	};

	me.validate = function() {
		return Validators.validate(me.command);
	};

	me.command = null;

	me.cancel = function() {
		me.command = null;
	};

	me.execute = function() {
		Api.command(me.command.jsonClass, me.command);
		me.cancel();
	};
}]);

app.controller("CommandCtrl", ["$scope", "Command", function($scope, Command) {
	$scope.Command = Command;
	var commandJson = "";
	$scope.$watch(function() {
		var newJson = JSON.stringify(Command.command);
		if(commandJson != newJson) {
			commandJson = newJson;
			$scope.validation = Command.validate();
		}
	});
}]);