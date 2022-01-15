ErrMsg <- "An error has occured in %s Error: %s";
enum CMDErrorTypes {
    UnknownCommand,
    InsufficientAuth,
    UnsupportedArg,
    IncompleteArgs,
    SyntaxError
}
CMD_Manager <-
{
    CommandsArray = []
    CommandsTable = {}


    BindFailListener = null
    BindFailEnv	  	 = null



    function Create(cmd, args_type, args_array, min_args, max_args, cmd_auth, protected, assoc) {
        try
        {
            local command = ::SqCommand(cmd, args_type, args_array, min_args, max_args, cmd_auth, protected, assoc);
            this.CommandsArray.push(command);
            this.CommandsTable.rawset(cmd.tolower(), command);
            return command;
        }
        catch(e) print(format(ErrMsg, "CMD_Manager::Create()", e));
    }
    function Run(author, level, text) {
        try
        {
            if(text != "")
            {
                local params = split(text, " ");
                local command = params[0];
                params.remove(0);
                local cmd = this.FindCmdByName(command);
                if(cmd != null)
                {
                    if(cmd.Protected == true && level < cmd.Authority) this.CallError(CMDErrorTypes.InsufficientAuth, author, command, "");
					else cmd.Run(author, params);
                }
                else this.CallError(CMDErrorTypes.UnknownCommand, author, command, "");
            }
        }
        catch(e) print(format(ErrMsg, "CMD_Manager::Run()", e));
    }
    function FindCmdByName(cmd) {
        if(this.CommandsTable.rawin(cmd.tolower())) return this.CommandsTable.rawget(cmd.tolower());
        return null;
    }
    function GetArray() {
        return this.CommandsArray;        
    }
    function GetTable() {
        return this.CommandsTable;        
    }

    function BindFail(environment, listener) {
		this.BindFailEnv	  = environment;
        this.BindFailListener = listener;
    }

    function CallError(type, author, cmd, msg)
    {
        if(this.BindFailListener != null)
        {
		this.BindFailListener.call(this.BindFailEnv, type, author, cmd, msg);
        }
    }

}

class SqCommand
{
    Name        = null;
    Authority   = -1;
    MinArgs     = 0;
    MaxArgs     = 0;
    Protected   = false;
    Assoc       = false;
    ArgsType    = null;
    ArgsName    = null;
    listener    = null;
	environment	= null;

    constructor(Name, Type, arguments, min, max, level, ibool_proc, ibool_assoc) {
        this.Name = Name;
        this.MinArgs = min;
        this.MaxArgs = max;
        this.Authority = level;
        this.Protected = ibool_proc;
        this.Assoc = ibool_assoc;

        this.ArgsType = split(Type, "|");
        this.ArgsName = arguments;
    }

    function BindExec(environment, listener) {
		this.environment = environment;
        this.listener = listener;
    }

    function Run(author, args) {
        try
        {
            if(args.len() >= this.MinArgs && args.len() == this.MaxArgs)
            {
                local err = false;
                local arguments;
                if(this.Assoc)
                {
                    arguments = {};
                }
                else
                {
                    arguments = [];
                }
                foreach(idx, i in args)
                {
                    local value = null;
                    if(i == null || i == "")
                    {
                        err = true;
                        break;
                    }
                    switch(this.ArgsType[idx])
                    {
                        case "b":
                            if(typeof GetArgValue(i) == "integer" && ["0", "1"].find(i) != null) value = i == "1" ? true : false;
                            if(typeof GetArgValue(i) == "bool") value = GetArgValue(i);
                        break;
                        case "g":
                            local arr = args.slice(idx, args.len());
                            value = arr.reduce(function (p, n) {
                                return p + " " + n;
                            });
                        break;
                        case "i":
                            if(typeof GetArgValue(i) == "integer") value = GetArgValue(i);
                        break;
                        case "f":
                            if(typeof GetArgValue(i) == "float") value = GetArgValue(i);
                        break;
                        case "s":
                           value = GetArgValue(i);
                        break;
                    }
                    if(value == null)
                    {
                        err = true;
                        break;
                    }
                    else
                    {
                        switch(this.Assoc)
                        {
                            case false:
                                arguments.push(value);
                            break;
                            case true:
                                arguments.rawset(this.ArgsName[idx], value);
                            break;
                        }
                        if(this.ArgsType[idx] == "g") 
                        {
                            break;
                        }
                    }
                }
                if(err) return CMD_Manager.CallError(CMDErrorTypes.UnsupportedArg, author, this.Name, "");
                else
                {
                    return this.RunFunction(author, arguments);
                }
            }
			return CMD_Manager.CallError(CMDErrorTypes.IncompleteArgs, author, this.Name, "");
        }
        catch(e) print(format(ErrMsg, "SqCommand::Create()", e));
    }
	
	
	function RunFunction(author, arguments)
	{
		try
		{
			this.listener.call(this.environment, author, arguments);
		}
		catch (e) {
			CMD_Manager.CallError(CMDErrorTypes.SyntaxError, author, this.Name, e);
			throw e;
		}
	}
}


enum ArgType { BOOL = 1, INTEGER = 2, FLOAT = 3, STRING = 4 }
function GetArgValue(arg)
{
    local type = arg.find(".") ? ArgType .FLOAT : ArgType .INTEGER;

    // Eliminate numeric types
    foreach (c in arg)
    {
        if ((c < 48 || c > 57) && c != 46) {
            type = ArgType .STRING;
            break;
        }
    }

    // Are we dealing with an intger?
    if (type == ArgType .INTEGER) {
        try {
            return arg.tointeger();
        } catch(e) {
            // Failed... Dfault to 0
            return 0;
        }
    // Are dealing with a real number?
    } else if (type == ArgType .FLOAT) {
        try {
            return arg.tofloat();
        } catch(e) {
            // Failed... Dfault to 0.0
            return 0.0;
        }
    // Area we toggling something?
    } else if (type == ArgType .STRING) {
        local arg2 = arg.tolower();
        if (arg2 == "on" || arg2 == "true" || arg2 == "enabled") {
            return true;
        } else if (arg2 == "off" || arg2 == "false" || arg2 == "disabled") {
            return false;
        }
    }
    // It's just a string.
    return arg;
}
