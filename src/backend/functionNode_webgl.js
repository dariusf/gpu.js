// Closure capture for the ast function, prevent collision with existing AST functions
var functionNode_webgl = (function() {

	var gpu, opt, jsFunctionString;

	function isIdentifierConstant(paramName) {
		if (!opt.constants) return false;
		return opt.constants.indexOf(paramName) != -1;
	}

	function isIdentifierKernelParam(paramName, ast, funcParam) {
		return funcParam.paramNames.indexOf(paramName) != -1;
	}

	function ensureIndentifierType(paramName, expectedType, ast, funcParam) {
		var start = ast.loc.start;

		if (!isIdentifierKernelParam(paramName, funcParam) && expectedType != 'float') {
			throw "Error unxpected identifier " + paramName + " on line " + start.line;
		} else {
			var actualType = funcParam.paramType[funcParam.paramNames.indexOf(paramName)];
			if (actualType != expectedType) {
				throw "Error unxpected identifier " + paramName + " on line " + start.line;
			}
		}
	}

	///
	/// Function: functionNode_webgl
	///
	/// [INTERNAL] Takes in a function node, and does all the AST voodoo required to generate its respective webGL code.
	///
	/// Parameter:
	/// 	inNode - {functionNode} The function node object
	///
	/// Returns:
	/// 	the converted webGL function string
	///
	function functionNode_webgl( inNode, _opt ) {
		gpu = inNode.gpu;
		opt = _opt || {};
		if (opt.debug) {
			console.log(inNode);
		}
		jsFunctionString = inNode.jsFunctionString;

		var env = {};
		infer(inNode.getJS_AST(), env);
		Object.keys(env).forEach(function (key) {
		    console.log(key + ':', printType(env[key]));
		});

		throw 'all good!';

		if (opt.prototypeOnly) {
			return ast_FunctionPrototype( inNode.getJS_AST(), [], inNode ).join("").trim();
		} else {
			inNode.webglFunctionString_array = ast_generic( inNode.getJS_AST(), [], inNode );
		}
		inNode.webglFunctionString = webgl_regex_optimize(
			inNode.webglFunctionString_array.join("").trim()
		);
		return inNode.webglFunctionString;
	}

	var DECODE32_ENCODE32 = /decode32\(\s+encode32\(/g;
	var ENCODE32_DECODE32 = /encode32\(\s+decode32\(/g;

	///
	/// Function: webgl_regex_optimize
	///
	/// [INTERNAL] Takes the near final webgl function string, and do regex search and replacments.
	/// For voodoo optimize out the following
	///
	/// - decode32(encode32(
	/// - encode32(decode32(
	///
	function webgl_regex_optimize( inStr ) {
		return inStr
			.replace(DECODE32_ENCODE32, "((")
			.replace(ENCODE32_DECODE32, "((")
		;
	}


	/// the AST error, with its location. To throw
	///
	/// @TODO: add location support fpr the AST error
	///
	/// @param error        the error message output
	/// @param ast          the AST object where the error is
	/// @param funcParam    FunctionNode, that tracks compilation state
	function ast_errorOutput(error, ast, funcParam) {
		console.error(error, ast, funcParam);
		return error;
	}

	/// Prases the abstract syntax tree, genericially to its respective function
	///
	/// @param ast          the AST object to parse
	/// @param retArr       return array string
	/// @param funcParam    FunctionNode, that tracks compilation state
	///
	/// @returns  the prased openclgl string array
	function ast_generic(ast, retArr, funcParam) {
		if(ast === null) {
			throw ast_errorOutput("NULL ast", ast, funcParam);
		} else {
			if (Array.isArray(ast)) {
				for (var i=0; i<ast.length; i++) {
					ast_generic(ast[i], retArr, funcParam);
				}
				return retArr;
			}

			switch(ast.type) {
				case "FunctionDeclaration":
					return ast_FunctionDeclaration(ast, retArr, funcParam);
				case "FunctionExpression":
					return ast_FunctionExpression(ast, retArr, funcParam);
				case "ReturnStatement":
					return ast_ReturnStatement(ast, retArr, funcParam);
				case "Literal":
					return ast_Literal(ast, retArr,  funcParam);
				case "BinaryExpression":
					return ast_BinaryExpression(ast, retArr,  funcParam);
				case "Identifier":
					return ast_IdentifierExpression(ast, retArr, funcParam);
				case "AssignmentExpression":
					return ast_AssignmentExpression(ast, retArr, funcParam);
				case "ExpressionStatement":
					return ast_ExpressionStatement(ast, retArr, funcParam);
				case "EmptyStatement":
					return ast_EmptyStatement(ast, retArr, funcParam);
				case "BlockStatement":
					return ast_BlockStatement(ast, retArr, funcParam);
				case "IfStatement":
					return ast_IfStatement(ast, retArr, funcParam);
				case "BreakStatement":
					return ast_BreakStatement(ast, retArr, funcParam);
				case "ContinueStatement":
					return ast_ContinueStatement(ast, retArr, funcParam);
				case "ForStatement":
					return ast_ForStatement(ast, retArr, funcParam);
				case "WhileStatement":
					return ast_WhileStatement(ast, retArr, funcParam);
				case "VariableDeclaration":
					return ast_VariableDeclaration(ast, retArr, funcParam);
				case "VariableDeclarator":
					return ast_VariableDeclarator(ast, retArr, funcParam);
				case "ThisExpression":
					return ast_ThisExpression(ast, retArr, funcParam);
				case "SequenceExpression":
					return ast_SequenceExpression(ast, retArr, funcParam);
				case "UnaryExpression":
					return ast_UnaryExpression(ast, retArr, funcParam);
				case "UpdateExpression":
					return ast_UpdateExpression(ast, retArr, funcParam);
				case "LogicalExpression":
					return ast_LogicalExpression(ast, retArr, funcParam);
				case "MemberExpression":
					return ast_MemberExpression(ast, retArr, funcParam);
				case "CallExpression":
					return ast_CallExpression(ast, retArr, funcParam);
				case "ArrayExpression":
					return ast_ArrayExpression(ast, retArr, funcParam);
			}

			throw ast_errorOutput("Unknown ast type : "+ast.type, ast, funcParam);
		}
	}

	/// Prases the abstract syntax tree, to its named function declartion
	///
	/// @param ast   the AST object to parse
	/// @param retArr       return array string
	/// @param funcParam    FunctionNode, that tracks compilation state
	///
	/// @returns  the appened retArr
	function ast_FunctionDeclaration(ast, retArr, funcParam) {
		// TODO: make this less hacky?
		var lines = jsFunctionString.split(/\r?\n/);

		var start = ast.loc.start;
		var end = ast.loc.end;

		var funcArr = [];

		funcArr.push(lines[start.line-1].slice(start.column));
		for (var i=start.line; i<end.line-1; i++) {
			funcArr.push(lines[i]);
		}
		funcArr.push(lines[end.line-1].slice(0,end.column));

		var funcStr = funcArr.join('\n');
		gpu.addFunction(funcStr);

		return retArr;
	}

	/// Prases the abstract syntax tree, to its named function prototype
	///
	/// @param ast   the AST object to parse
	/// @param retArr       return array string
	/// @param funcParam    FunctionNode, that tracks compilation state
	///
	/// @returns  the appened retArr
	function ast_FunctionPrototype(ast, retArr, funcParam) {
		// Setup function return type and name
		if(funcParam.isRootKernel) {
			return retArr;
		}

		retArr.push(funcParam.returnType);
		retArr.push(" ");
		retArr.push(funcParam.functionName);
		retArr.push("(");

		// Arguments handling
		for( var i = 0; i < funcParam.paramNames.length; ++i ) {
			if( i > 0 ) {
				retArr.push(", ");
			}

			retArr.push( funcParam.paramType[i] );
			retArr.push(" ");
			retArr.push("user_");
			retArr.push( funcParam.paramNames[i] );
		}

		retArr.push(");\n");

		return retArr;
	}

	/// Prases the abstract syntax tree, to its named function
	///
	/// @param ast   the AST object to parse
	/// @param retArr       return array string
	/// @param funcParam    FunctionNode, that tracks compilation state
	///
	/// @returns  the appened retArr
	function ast_FunctionExpression(ast, retArr, funcParam) {

		// Setup function return type and name
		if(funcParam.isRootKernel) {
			retArr.push("void");
			funcParam.kernalAst = ast;
		} else {
			retArr.push(funcParam.returnType);
		}
		retArr.push(" ");
		retArr.push(funcParam.functionName);
		retArr.push("(");

		if(!funcParam.isRootKernel) {
			// Arguments handling
			for( var i = 0; i < funcParam.paramNames.length; ++i ) {
				if( i > 0 ) {
					retArr.push(", ");
				}

				retArr.push( funcParam.paramType[i] );
				retArr.push(" ");
				retArr.push("user_");
				retArr.push( funcParam.paramNames[i] );
			}
		}

		// Function opening
		retArr.push(") {\n");

		// Body statement iteration
		for(var i=0; i<ast.body.length; ++i) {
			ast_generic(ast.body[i], retArr, funcParam);
			retArr.push("\n");
		}

		// Function closing
		retArr.push("}\n");
		return retArr;
	}

	/// Prases the abstract syntax tree, to return function
	///
	/// @param ast          the AST object to parse
	/// @param retArr       return array string
	/// @param funcParam    FunctionNode, that tracks compilation state
	///
	/// @returns  the appened retArr
	function ast_ReturnStatement(ast, retArr, funcParam) {
		if(funcParam.isRootKernel) {
			retArr.push("kernelResult = ");
			ast_generic(ast.argument, retArr, funcParam);
			retArr.push(";");
			retArr.push("return;");
		} else {
			retArr.push("return ");
			ast_generic(ast.argument, retArr, funcParam);
			retArr.push(";");
		}

		//throw ast_errorOutput(
		//	"Non main function return, is not supported : "+funcParam.currentFunctionNamespace,
		//	ast, funcParam
		//);

		return retArr;
	}

	/// Prases the abstract syntax tree, literal value
	///
	/// @param ast          the AST object to parse
	/// @param retArr       return array string
	/// @param funcParam    FunctionNode, that tracks compilation state
	///
	/// @returns  the appened retArr
	function ast_Literal(ast, retArr, funcParam) {

		// Reject non numeric literals
		if( isNaN(ast.value) ) {
			throw ast_errorOutput(
				"Non-numeric literal not supported : "+ast.value,
				ast, funcParam
			);
		}

		// Push the literal value as a float/int
		retArr.push( ast.value );

		// If it was an int, node made a float
		if( Number.isInteger(ast.value) ) {
			retArr.push(".0");
		}

		return retArr;
	}

	/// Prases the abstract syntax tree, binary expression
	///
	/// @param ast          the AST object to parse
	/// @param retArr       return array string
	/// @param funcParam    FunctionNode, that tracks compilation state
	///
	/// @returns  the appened retArr
	function ast_BinaryExpression(ast, retArr, funcParam) {
		retArr.push("(");

		if (ast.operator == "%") {
			retArr.push("mod(");
			ast_generic(ast.left, retArr, funcParam);
			retArr.push(",");
			ast_generic(ast.right, retArr, funcParam);
			retArr.push(")");
		} else if (ast.operator == "===") {
			ast_generic(ast.left, retArr, funcParam);
			retArr.push("==");
			ast_generic(ast.right, retArr, funcParam);
		} else if (ast.operator == "!==") {
			ast_generic(ast.left, retArr, funcParam);
			retArr.push("!=");
			ast_generic(ast.right, retArr, funcParam);
		} else {
			ast_generic(ast.left, retArr, funcParam);
			retArr.push(ast.operator);
			ast_generic(ast.right, retArr, funcParam);
		}

		retArr.push(")");

		return retArr;
	}

	/// Prases the abstract syntax tree, identifier expression
	///
	/// @param ast          the AST object to parse
	/// @param retArr       return array string
	/// @param funcParam    FunctionNode, that tracks compilation state
	///
	/// @returns  the appened retArr
	function ast_IdentifierExpression(idtNode, retArr, funcParam) {
		if (idtNode.type != "Identifier") {
			throw ast_errorOutput(
				"IdentifierExpression - not an Identifier",
				ast, funcParam
			);
		}

		if (idtNode.name == "gpu_threadX") {
			retArr.push('threadId.x');
		} else if (idtNode.name == "gpu_threadY") {
			retArr.push('threadId.y');
		} else if (idtNode.name == "gpu_threadZ") {
			retArr.push('threadId.z');
		} else if (idtNode.name == "gpu_dimensionsX") {
			retArr.push('uOutputDim.x');
		} else if (idtNode.name == "gpu_dimensionsY") {
			retArr.push('uOutputDim.y');
		} else if (idtNode.name == "gpu_dimensionsZ") {
			retArr.push('uOutputDim.z');
		} else {
			retArr.push("user_"+idtNode.name);
		}

		return retArr;
	}

	/// Prases the abstract syntax tree, genericially to its respective function
	///
	/// @param ast   the AST object to parse
	///
	/// @returns  the prased openclgl string
	function ast_ForStatement(forNode, retArr, funcParam) {
		if (forNode.type != "ForStatement") {
			throw ast_errorOutput(
				"Invalid for statment",
				ast, funcParam
			);
		}

		if (forNode.test && forNode.test.type == "BinaryExpression") {
			if (forNode.test.right.type == "Identifier"
				&& forNode.test.operator == "<"
				&& isIdentifierConstant(forNode.test.right.name) == false) {

				if (opt.loopMaxIterations === undefined) {
					console.warn("Warning: loopMaxIterations is not set! Using default of 100 which may result in unintended behavior.");
					console.warn("Set loopMaxIterations or use a for loop of fixed length to silence this message.");
				}

				retArr.push("for (float ");
				ast_generic(forNode.init, retArr, funcParam);
				retArr.push(";");
				ast_generic(forNode.test.left, retArr, funcParam);
				retArr.push(forNode.test.operator);
				retArr.push("LOOP_MAX");
				retArr.push(";");
				ast_generic(forNode.update, retArr, funcParam);
				retArr.push(")");

				retArr.push("{\n");
				retArr.push("if (");
				ast_generic(forNode.test.left, retArr, funcParam);
				retArr.push(forNode.test.operator);
				ast_generic(forNode.test.right, retArr, funcParam);
				retArr.push(") {\n");
				if (forNode.body.type == "BlockStatement") {
					for (var i = 0; i < forNode.body.body.length; i++) {
						ast_generic(forNode.body.body[i], retArr, funcParam);
					}
				} else {
					ast_generic(forNode.body, retArr, funcParam);
				}
				retArr.push("} else {\n");
				retArr.push("break;\n");
				retArr.push("}\n");
				retArr.push("}\n");

				return retArr;
			} else {
				retArr.push("for (float ");
				ast_generic(forNode.init, retArr, funcParam);
				retArr.push(";");
				ast_generic(forNode.test, retArr, funcParam);
				retArr.push(";");
				ast_generic(forNode.update, retArr, funcParam);
				retArr.push(")");
				ast_generic(forNode.body, retArr, funcParam);
				return retArr;
			}
		}

		throw ast_errorOutput(
			"Invalid for statment",
			ast, funcParam
		);
	}

	/// Prases the abstract syntax tree, genericially to its respective function
	///
	/// @param ast   the AST object to parse
	///
	/// @returns  the prased openclgl string
	function ast_WhileStatement(whileNode, retArr, funcParam) {
		if (whileNode.type != "WhileStatement") {
			throw ast_errorOutput(
				"Invalid while statment",
				ast, funcParam
			);
		}

		retArr.push("for (float i=0.0; i<LOOP_MAX; i++) {");
		retArr.push("if (");
		ast_generic(whileNode.test, retArr, funcParam);
		retArr.push(") {\n");
		ast_generic(whileNode.body, retArr, funcParam);
		retArr.push("} else {\n");
		retArr.push("break;\n");
		retArr.push("}\n");
		retArr.push("}\n");

		return retArr;
	}

	function ast_AssignmentExpression(assNode, retArr, funcParam) {
		if(assNode.operator == "%=") {
			ast_generic(assNode.left, retArr, funcParam);
			retArr.push("=");
			retArr.push("mod(");
			ast_generic(assNode.left, retArr, funcParam);
			retArr.push(",");
			ast_generic(assNode.right, retArr, funcParam);
			retArr.push(")");
		} else {
			ast_generic(assNode.left, retArr, funcParam);
			retArr.push(assNode.operator);
			ast_generic(assNode.right, retArr, funcParam);
			return retArr;
		}
	}

	function ast_EmptyStatement(eNode, retArr, funcParam) {
		retArr.push(";\n");
		return retArr;
	}

	function ast_BlockStatement(bNode, retArr, funcParam) {
		retArr.push("{\n");
		for (var i = 0; i < bNode.body.length; i++) {
			ast_generic(bNode.body[i], retArr, funcParam);
		}
		retArr.push("}\n");
		return retArr;
	}

	function ast_ExpressionStatement(esNode, retArr, funcParam) {
		ast_generic(esNode.expression, retArr, funcParam);
		retArr.push(";\n");
		return retArr;
	}

	function ast_VariableDeclaration(vardecNode, retArr, funcParam) {
		retArr.push("float ");
		for (var i = 0; i < vardecNode.declarations.length; i++) {
			if (i > 0) {
				retArr.push(",");
			}
			ast_generic(vardecNode.declarations[i], retArr, funcParam);
		}
		retArr.push(";");
		return retArr;
	}

	function ast_VariableDeclarator(ivardecNode, retArr, funcParam) {

		ast_generic(ivardecNode.id, retArr, funcParam);
		if (ivardecNode.init !== null) {
			retArr.push("=");
			ast_generic(ivardecNode.init, retArr, funcParam);
		}
		return retArr;
	}

	function ast_IfStatement(ifNode, retArr, funcParam) {
		retArr.push("if(");
		ast_generic(ifNode.test, retArr, funcParam);
		retArr.push(")");
		if (ifNode.consequent.type == "BlockStatement") {
			ast_generic(ifNode.consequent, retArr, funcParam);
		} else {
			retArr.push(" {\n");
			ast_generic(ifNode.consequent, retArr, funcParam);
			retArr.push("\n}\n");
		}

		if (ifNode.alternate) {
			retArr.push("else ");
			if (ifNode.alternate.type == "BlockStatement") {
				ast_generic(ifNode.alternate, retArr, funcParam);
			} else {
				retArr.push(" {\n");
				ast_generic(ifNode.alternate, retArr, funcParam);
				retArr.push("\n}\n");
			}
		}
		return retArr;

	}

	function ast_BreakStatement(brNode, retArr, funcParam) {
		retArr.push("break;\n");
		return retArr;
	}

	function ast_ContinueStatemnt(crNode, retArr, funcParam) {
		retArr.push("continue;\n");
		return retArr;
	}

	function ast_LogicalExpression(logNode, retArr, funcParam) {
		retArr.push("(");
		ast_generic(logNode.left, retArr, funcParam);
		retArr.push(logNode.operator);
		ast_generic(logNode.right, retArr, funcParam);
		retArr.push(")");
		return retArr;
	}

	function ast_UpdateExpression(uNode, retArr, funcParam) {
		if(uNode.prefix) {
			retArr.push(uNode.operator);
			ast_generic(uNode.argument, retArr, funcParam);
		} else {
			ast_generic(uNode.argument, retArr, funcParam);
			retArr.push(uNode.operator);
		}

		return retArr;
	}

	function ast_UnaryExpression(uNode, retArr, funcParam) {
		if(uNode.prefix) {
			retArr.push(uNode.operator);
			ast_generic(uNode.argument, retArr, funcParam);
		} else {
			ast_generic(uNode.argument, retArr, funcParam);
			retArr.push(uNode.operator);
		}

		return retArr;
	}

	function ast_ThisExpression(tNode, retArr, funcParam) {
		retArr.push("this");
		return retArr;
	}

	// The prefixes to use
	var jsMathPrefix = "Math.";
	var localPrefix = "this.";
	var constantsPrefix = "this.constants.";

	function ast_MemberExpression(mNode, retArr, funcParam) {
		if(mNode.computed) {
			if (mNode.object.type == "Identifier") {
				// Working logger
				var reqName = mNode.object.name;
				var funcName = funcParam.funcName || "kernel";
				var assumeNotTexture = false;

				// Possibly an array request - handle it as such
				if(funcParam != "kernel" && funcParam.paramNames ) {
					var idx = funcParam.paramNames.indexOf(reqName);
					if( idx >= 0 && funcParam.paramType[idx] == "float") {
						assumeNotTexture = true;
					}
				}

				if(assumeNotTexture) {
					// Get from array
					ast_generic(mNode.object, retArr, funcParam);
					retArr.push("[int(");
					ast_generic(mNode.property, retArr, funcParam);
					retArr.push(")]");

					//console.log(mNode.property.operator);
				} else {
					// Get from texture
					// This normally refers to the global read only input vars
					retArr.push("get(");
					ast_generic(mNode.object, retArr, funcParam);
					retArr.push(", vec2(");
					ast_generic(mNode.object, retArr, funcParam);
					retArr.push("Size[0],");
					ast_generic(mNode.object, retArr, funcParam);
					retArr.push("Size[1]), vec3(");
					ast_generic(mNode.object, retArr, funcParam);
					retArr.push("Dim[0],");
					ast_generic(mNode.object, retArr, funcParam);
					retArr.push("Dim[1],");
					ast_generic(mNode.object, retArr, funcParam);
					retArr.push("Dim[2]");
					retArr.push("), ");
					ast_generic(mNode.property, retArr, funcParam);
					retArr.push(")");
				}
			} else {
				ast_generic(mNode.object, retArr, funcParam);
				var last = retArr.pop();
				retArr.push(",");
				ast_generic(mNode.property, retArr, funcParam);
				retArr.push(")");

				//console.log(mNode.property.operator);
			}
		} else {

			// Unroll the member expression
			var unrolled = ast_MemberExpression_unroll(mNode);
			var unrolled_lc = unrolled.toLowerCase();

			// Its a constant, remove this.constants.
			if( unrolled.indexOf(constantsPrefix) === 0 ) {
				unrolled = 'constants_'+unrolled.slice( constantsPrefix.length );
			}

			if (unrolled_lc == "this.thread.x") {
				retArr.push('threadId.x');
			} else if (unrolled_lc == "this.thread.y") {
				retArr.push('threadId.y');
			} else if (unrolled_lc == "this.thread.z") {
				retArr.push('threadId.z');
			} else if (unrolled_lc == "this.dimensions.x") {
				retArr.push('uOutputDim.x');
			} else if (unrolled_lc == "this.dimensions.y") {
				retArr.push('uOutputDim.y');
			} else if (unrolled_lc == "this.dimensions.z") {
				retArr.push('uOutputDim.z');
			} else {
				retArr.push(unrolled);
			}
		}
		return retArr;
	}

	function ast_SequenceExpression(sNode, retArr, funcParam) {
		for (var i = 0; i < sNode.expressions.length; i++) {
			if (i > 0) {
				retArr.push(",");
			}
			ast_generic(sNode.expressions, retArr, funcParam);
		}
		return retArr;
	}

	/// Utility function for ast_CallExpression.
	///
	/// Prases the abstract syntax tree, binary expression.
	///
	/// @param ast          the AST object to parse
	///
	/// @returns  {String} the function namespace call, unrolled
	function ast_MemberExpression_unroll(ast, funcParam) {
		if( ast.type == "Identifier" ) {
			return ast.name;
		} else if( ast.type == "ThisExpression" ) {
			return "this";
		}

		if( ast.type == "MemberExpression" ) {
			if( ast.object && ast.property ) {
				return (
					ast_MemberExpression_unroll( ast.object, funcParam ) +
					"." +
					ast_MemberExpression_unroll( ast.property, funcParam )
				);
			}
		}

		// Failure, unknown expression
		throw ast_errorOutput(
			"Unknown CallExpression_unroll",
			ast, funcParam
		);
	}

	/// Prases the abstract syntax tree, binary expression
	///
	/// @param ast          the AST object to parse
	/// @param retArr       return array string
	/// @param funcParam    FunctionNode, that tracks compilation state
	///
	/// @returns  the appened retArr
	function ast_CallExpression(ast, retArr, funcParam) {
		if( ast.callee ) {
			// Get the full function call, unrolled
			var funcName = ast_MemberExpression_unroll(ast.callee);

			// Its a math operator, remove the prefix
			if( funcName.indexOf(jsMathPrefix) === 0 ) {
				funcName = funcName.slice( jsMathPrefix.length );
			}

			// Its a local function, remove this
			if( funcName.indexOf(localPrefix) === 0 ) {
				funcName = funcName.slice( localPrefix.length );
			}

			// Register the function into the called registry
			if( funcParam.calledFunctions.indexOf(funcName) < 0 ) {
				funcParam.calledFunctions.push(funcName);
			}

			// Call the function
			retArr.push( funcName );

			// Open arguments space
			retArr.push( "(" );

			// Add the vars
			for(var i=0; i<ast.arguments.length; ++i) {
				if(i > 0) {
					retArr.push(", ");
				}
				ast_generic(ast.arguments[i],retArr,funcParam);
			}

			// Close arguments space
			retArr.push( ")" );

			return retArr;
		}

		// Failure, unknown expression
		throw ast_errorOutput(
			"Unknown CallExpression",
			ast, funcParam
		);

		return retArr;
	}

	/// Prases the abstract syntax tree, Array Expression
	///
	/// @param ast          the AST object to parse
	/// @param retArr       return array string
	/// @param funcParam    FunctionNode, that tracks compilation state
	///
	/// @returns  the appened retArr
	function ast_ArrayExpression(arrNode, retArr, funcParam) {
		// console.log(arrNode);
		var arrLen = arrNode.elements.length;

		retArr.push("float["+arrLen+"](");
		for(var i=0; i<arrLen; ++i) {
			if(i > 0) {
				retArr.push(", ");
			}
			var subNode = arrNode.elements[i];
			ast_generic(subNode, retArr, funcParam)
		}
		retArr.push(")");

		return retArr;

		// // Failure, unknown expression
		// throw ast_errorOutput(
		// 	"Unknown  ArrayExpression",
		// 	arrNode, funcParam
		// );
	}

	/// Prints a type variable. Types are of the following forms:
	///
	/// {type: 'term', value: 'float'}
	/// {type: 'mono', value: 'a'}
	/// {type: 'constr', name: 'list', args: [ ... more types ]}
	///
	/// Arrow and polytypes are not yet supported.
	///
	/// @param type
	/// @return string representation of the type
	function printType(type) {
	    switch (type.type) {
	    case 'term':
	        return type.value;
	    case 'mono':
	        return "'" + type.value;
	    case 'constr':
	        return type.name + '(' + type.args.map(printType).join(', ') + ')';
	    case 'var':
	        return "~" + type.value;
	    case 'arrow':
	        return type.args.map(printType).join(' -> ');
	    default:
	        throw 'unrecognised type ' + JSON.stringify(type);
	    }
	}

	/// Destructively unifies two types, throwing if that fails.
	///
	/// @param t1
	/// @param t2
	/// @return nothing
	function unify(t1, t2) {
	    if (t1.type === 'term' && t2.type === 'term') {
	        if (t1.value !== t2.value) {
	            throw 'could not unify ' + printType(t1) + ' and ' + printType(t2);
	        }
	    } else if (t1.type === 'constr' && t2.type === 'constr') {
	        if (t1.name !== t2.name || t1.args.length !== t2.args.length) {
	            throw 'could not unify constructors ' + printType(t1) + ' and ' + printType(t2);
	        }
	        for (var i=0; i<t1.args.length; i++) {
	            unify(t1.args[i], t2.args[i]);
	        }
	    } else if (t1.type === 'mono' && t2.type === 'mono') {
	        if (t1.value === t2.value) {
	            throw 'there can only be a single instance of a monotype'
	        }
	    } else if (t1.type === 'mono') {
	        if (t2.type !== 'mono') {
	            // TODO there's probably a better way to do this
	            Object.keys(t1).forEach(function (key) {
	                delete t1[key];
	            });
	            Object.keys(t2).forEach(function (key) {
	                t1[key] = t2[key];
	            });
	        } else {
	            console.assert(false);
	        }
	    } else if (t2.type === 'mono') {
	        unify(t2, t1);
	    } else {
	        throw 'could not unify ' + JSON.stringify(t1) + ' and ' + JSON.stringify(t2);
	    }
	}

	function fresh() {
	    return fresh.ness++;
	}
	fresh.ness = 0;

	/// Infers types of an AST subtree given a context, mutating it with types.
	///
	/// Contexts are just (string, type) mappings, and an empty object is
	/// suitable when calling this for the first time. It is mutated as we go.
	///
	/// @param ast          AST subtree to infer types for
	/// @param env          context
	/// @returns            nothing
	function infer(ast, env) {
	    if(ast === null) {
	        throw ast_errorOutput("NULL ast", ast, funcParam);
	    }

	    if (Array.isArray(ast)) {
	        ast.forEach(function (node) {
	            infer(node, env);
	        });
	        return;
	    }

	    switch(ast.type) {
	    case "FunctionDeclaration":
	    case "FunctionExpression":
	        ast.params.forEach(function (a) {
	            env[a.name] = {type: 'mono', value: fresh()};
	        });
	        infer(ast.body, env);
	        break;
	    case "Literal":
	        if (typeof ast.value === 'boolean') {
	            ast.inferred_type = {type: 'term', value: 'bool'};
	        } else {
	            ast.inferred_type = {type: 'term', value: 'float'};
	        }
	        break;
	    case "Identifier":
	        if (env[ast.name]) {
	            ast.inferred_type = env[ast.name];
	        } else {
	            throw ast.name + ' does not seem to have been defined yet!';
	        }
	        break;
	    case "VariableDeclaration":
	        ast.declarations.forEach(function (v) {
	            infer(v, env);
	        });
	        break;
	    case "VariableDeclarator":
	        infer(ast.init, env);
	        if (env[ast.id.name]) {
	            unify(env[ast.id.name], ast.init.inferred_type);
	        } else {
	            env[ast.id.name] = ast.init.inferred_type;
	        }
	        break;
	    case "BinaryExpression":
	        switch (ast.operator) {
	        case '+':
	        case '-':
	            infer(ast.left, env);
	            infer(ast.right, env);
	            unify(ast.left.inferred_type, {type: 'term', value: 'float'});
	            unify(ast.right.inferred_type, {type: 'term', value: 'float'});
	            ast.inferred_type = {type: 'term', value: 'float'};
	            break;
	        default:
	            throw 'operator ' + ast.operator + ' not yet supported in binary expression'
	        }
	        break;
	    case "LogicalExpression":
	        switch (ast.operator) {
	        case '&&':
	        case '||':
	            infer(ast.left, env);
	            infer(ast.right, env);
	            unify(ast.left.inferred_type, {type: 'term', value: 'bool'});
	            unify(ast.right.inferred_type, {type: 'term', value: 'bool'});
	            ast.inferred_type = {type: 'term', value: 'bool'};
	            break;
	        default:
	            throw 'operator ' + ast.operator + 'not yet supported in logical expression';
	        }
	        break;
	    case "MemberExpression":
	        if (ast.computed) {
	            infer(ast.object, env);
	            var inner = {type: 'mono', value: fresh()};
	            unify(ast.object.inferred_type, {
	                type: 'constr',
	                name: 'array',
	                args: [inner]
	            });
	            infer(ast.property, env);
	            unify(ast.property.inferred_type, {type: 'term', value: 'float'})
	            ast.inferred_type = inner;
	        } else {
	            switch (ast.property.name) {
	            case 'x': case 'y': case 'z': case 'w':
	                infer(ast.object, env);
	                if (ast.object.inferred_type.type === 'constr') {
	                    switch (ast.object.inferred_type.name) {
	                    case 'vec2':
	                        if (ast.property.name === 'z' || ast.property.name === 'w') {
	                            throw 'vec2 cannot be indexed with .' + ast.property.name;
	                        }
	                        ast.inferred_type = ast.object.inferred_type.args[0];
	                        break;
	                    }
	                } else {
	                    throw 'cannot use .' + ast.property.name + ' to index a ' + printType(ast.object.inferred_type);
	                }
	                break;
	            default:
	                throw 'arbitrary property access (.' + ast.property.name + ') is not yet supported'
	            }
	        }
	        break;
	    case "CallExpression":
	        // TODO generalise for user-defined functions
	        switch (ast.callee.name) {
	        case 'vec2':
	            if (ast.arguments.length !== 2) {
	                throw 'invalid number of args for function ' + ast.callee.name;
	            }
	            var v = {type: 'mono', value: fresh()};
	            ast.arguments.forEach(function (arg) {
	                infer(arg, env);
	                unify(arg.inferred_type, v);
	            });
	            ast.inferred_type = {type: 'constr', name: 'vec2', args: [v]};
	            break;
	        default:
	            throw 'unrecognised function ' + ast.callee.name;
	        }
	        break;
	    case "AssignmentExpression":
	        infer(ast.left, env);
	        infer(ast.right, env);
	        unify(ast.left.inferred_type, ast.right.inferred_type);
	        break;
	    case "BlockStatement":
	        infer(ast.body, env);
	        break;
	    case "ExpressionStatement":
	        infer(ast.expression, env);
	        break;
	    case "ReturnStatement":
	        infer(ast.argument, env);
	        break;
	    case "EmptyStatement":
	    case "BreakStatement":
	    case "ContinueStatement":
	        break;
	    default:
	        throw '// not implemented: ' + ast.type + ' ' + JSON.stringify(ast);
	    }
	}

	return functionNode_webgl;
})();
