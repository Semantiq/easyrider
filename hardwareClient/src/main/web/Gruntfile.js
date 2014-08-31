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
		less: {
			build: {
				files: {
					"../resources/static/main.css": "styles/main.less"
				}
			}
		},
		peg: {
			build: {
				src: "grammars/search.peg",
				dest: "../resources/static/search.js",
				options: {
					angular: {
						module: "easyriderSearch",
						factory: "SearchGrammar"
					}
				}
			}
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
	grunt.loadNpmTasks('grunt-contrib-less');
	grunt.loadNpmTasks('grunt-peg');

	grunt.registerTask('default', ['jshint', 'uglify', "copy"]);
};