app.constant('Utils', {
	isValidId: function(id) { return /^[a-zA-Z0-9_]+$/.test(id); },
	idInfo: "Id must be created from uppercase and lowercase letters, digits and underscore symbol."
});