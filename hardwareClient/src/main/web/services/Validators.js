app.service("Validators", function() {
	var me = this;

	var validators = {};

	me.addValidator = function(commandType, fun) {
		if(!validators[commandType])
			validators[commandType] = [];
		validators[commandType].push(fun);
	};

	me.validate = function(command) {
		if(!command)
			return {
				alerts: [],
				canExplain: false,
				canExecute: false
			};

		var alerts = [];
		var canExplain = true;
		var canExecute = true;

		var reporter = {
			noExecute: function() {
				canExecute = false;
			},
			noExplain: function() {
				canExplain = false;
				canExecute = false;
			},
			info: function(title, text) {
				alerts.push({
					level: "info",
					title: title,
					text: text
				});
			},
			warn: function(title, text) {
				alerts.push({
					level: "warning",
					title: title,
					text: text
				});
			},
			fail: function(title, text) {
				alerts.push({
					level: "danger",
					title: title,
					text: text
				});
				reporter.noExplain();
			}
		};

		var lst = validators[command.jsonClass];

		for(var i in lst) {
			lst[i](command, reporter);
		}

		return {
			canExecute: canExecute,
			canExplain: canExplain,
			alerts: alerts
		};
	};
});