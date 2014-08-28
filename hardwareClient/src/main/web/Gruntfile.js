module.exports = function(grunt) {

	grunt.initConfig({
		pkg: grunt.file.readJSON('package.json'),
		uglify: {
			options: {
				banner: '/*! EasyRider build <%= grunt.template.today("dd-mm-yyyy") %> */\n',
				beautify: true
			},
			dist: {
				files: {
					'../resources/static/main.min.js': [
						'js/init.js',
						'services/**/*.js',
						'pages/**/*.js',
						'directives/**/*.js'
					]
				}
			}
		},
		jshint: {
			files: ['Gruntfile.js', 'js/init.js', 'services/**/*.js', 'pages/**/*.js'],
		},
		copy: {
			main: {
				files: [
					{ expand: true, flatten: false, src: ["pages/**/*.html"], dest: "../resources/static" },
					{ expand: true, flatten: false, src: ["directives/**/*.html"], dest: "../resources/static" },
					{ expand: true, flatten: false, src: ["commands/**/*.html"], dest: "../resources/static" },
					{ expand: true, flatten: false, src: ["bower_components/**"], dest: "../resources/static/vendor" },
					{ expand: true, flatten: false, src: ["vendor/**"], dest: "../resources/static/vendor" },
					{ expand: true, flatten: true, src: ["html/index.html"], dest: "../resources/static" }
				]
			}
		}
	});

	grunt.loadNpmTasks('grunt-contrib-uglify');
	grunt.loadNpmTasks('grunt-contrib-jshint');
	grunt.loadNpmTasks('grunt-contrib-copy');

	grunt.registerTask('default', ['jshint', 'uglify', "copy"]);
};