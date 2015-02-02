module.exports = function(grunt) {

	grunt.initConfig({
		pkg: grunt.file.readJSON('package.json'),
		uglify: {
			options: {
				banner: '/*! EasyRider build <%= grunt.template.today("dd-mm-yyyy") %> */\n',
				compress: false,
				mangle: false,
				beautify: true
			},
			dist: {
				files: {
					    '../resources/static/ssh.min.js': [
						'directives/**/*.js',
						'pages/**/*.js',
						'extensions/**/*.js'
					]
				}
			}
		},
		jshint: {
			files: ['Gruntfile.js', 'js/init.js', 'services/*.js', 'pages/**/*.js'],
		},
		less: {
			build: {
				files: {
					"../resources/static/main.css": "styles/main.less"
				}
			}
		},
		copy: {
			main: {
				files: [
					{ expand: true, flatten: false, src: ["pages/**/*.html"], dest: "../resources/static" },
					{ expand: true, flatten: false, src: ["directives/**/*.html"], dest: "../resources/static" },
					{ expand: true, flatten: false, src: ["extensions/**/*.html"], dest: "../resources/static" },
				]
			}
		}
	});

	grunt.loadNpmTasks('grunt-contrib-uglify');
	grunt.loadNpmTasks('grunt-contrib-jshint');
	grunt.loadNpmTasks('grunt-contrib-copy');
	grunt.loadNpmTasks('grunt-contrib-less');

	grunt.registerTask('default', ['jshint', 'uglify', "copy", "less"]);
};
