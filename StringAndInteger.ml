open Ast


let printLn =
{
	name_method = "println";
	param_method = [];
	body_method =
		{
			declarations = [];
			instructions = []
		};
	is_static_method = false;
	is_override = false;
	return_type = None
}
let printMeth =
{
	name_method = "print";
	param_method = [];
	body_method =
		{
			declarations = [];
			instructions = []
		};
	is_static_method = false;
	is_override = false;
	return_type = None
}


let stringClass =
{
	name_class = "String";
	params_class = [];
	superclass = None;
	constructor =
		{
			name_constructor = "String";
			param_constructor = [];
			body_constructor =
				{
					declarations = [];
					instructions = [];
				};
			super_call = None;
		};
	attributes = [];
	methods = [printMeth; printLn]
}













let intToStringMethod =
{
	name_method = "toString";
	param_method = [];
	is_static_method = false;
	is_override = false;
	return_type = Some "String";
	body_method =
		{
			declarations = [
				{
					name = "result";
					is_var = false;
					is_static = false;
					typ = "String"
				}
			];
			instructions =
				[
					Affectation(LocalVar("result"), StringLiteral("UNDEFINED"));
					Return
				]
		}
}


let integerClass =
{
	name_class = "Integer";
	params_class = [];
	superclass = None;
	constructor =
		{
			name_constructor = "Integer";
			param_constructor = [];
			body_constructor =
				{
					declarations = [];
					instructions = [];
				};
			super_call = None
		};
	attributes = [];
	methods = [intToStringMethod];
}