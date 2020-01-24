# Defined in - @ line 1
function rmbf --description alias\ rmbf=rm\ -f\ \*\~\ .\*\~\ \\\#\*
	rm -f *~ .*~ \#* $argv;
end
