# Defined in - @ line 1
function pg --description 'alias pg=ps -ef | grep'
	ps -ef | grep $argv;
end
