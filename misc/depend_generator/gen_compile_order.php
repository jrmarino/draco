<?php
/**
* @desc This script loads in a dependency list and an object list, although the object list
* is likely a subset of the dependency list.  The output is a reordered object list such that
* the dependencies of the later objects are compiled previous (so the first objects have no
* dependencies and the last objects have the most.
*
* input: deplist: basename (example: deplist=gnat1bind loads dep.gnat1bind.txt)
*        objlist: basename (example: objlist=gnat1 loads obj.gnat1.txt)
*
* output: tbw
*
* This script should work as a cli tool as well as a web-based one.
*/

$arguments = isset($argv) ? $argv : array();
$numArgs = count($arguments);
for ($i = 1; $i < $numArgs; $i++)
{
	parse_str($arguments[$i]);
}

$inputs = array ('deplist' => null, 'objlist' => null);
$options = compact('deplist','objlist');
$onweb = false;

foreach (array_keys ($inputs) as $index)
{
	if (isset($options[$index]))
	{
		$inputs[$index] = $options['deplist'];
	}
	else if (isset ($_GET[$index]))
	{
		$inputs[$index] = $_GET[$index];
		$onweb = true;
	}
	else
	{
		die ("The input '$index' was not provided.  Aborting....");
	}
}

$workdir = dirname (__FILE__);
$depfile = "$workdir/dep." . $inputs['deplist'] . '.txt';
$objfile = "$workdir/obj." . $inputs['objlist'] . '.txt';

if (!file_exists($depfile))
{
	die ("The file '$depfile' does not exist.  Aborting...");
}
else if (!file_exists($objfile))
{
	die ("The file '$objfile' does not exist.  Aborting...");
}

/**
* @desc Data struction of $srcList
* array (
*    0 => number of dependencies
*    1 => base file name    (all are objects, .o extension)
*    2 => relative path     (proably all "ada")
*    3 => array of array (
*                    0 => basename
*                    1 => extension
*                    2 => relative path (probabably all "ada"
*                  )
*         )
* )
*
*/

define ( "N1_NUMDEP",    0);
define ( "N1_BASENAME",  1);
define ( "N1_RELPATH",   2);
define ( "N1_ORIGPOS",   3);
define ( "N1_DEPENDS",   4);

function depSort ($a, $b)
{
	if ($a[N1_NUMDEP] == $b[N1_NUMDEP])
	{
		return strcmp($a[N1_BASENAME], $b[N1_BASENAME]);
	}
	return $a[N1_NUMDEP] > $b[N1_NUMDEP] ? 1 : -1;
}

class TSortAda
{
	private $srcList       = array ();
	private $sortedSrcList = array ();
	private $spec_has_body = array ();
	private $fileset_built = array ();
	private $orderedList   = array ();
	private $completeList  = array ();
	private $objectList    = array ();
	private $separate_comp = array ();
	private $missing_objs  = array ();
	private $unused_files  = array ();
	private $component;



	function __construct ($depfile, $objfile, $pass_component)
	{
		$this->component = $pass_component;
		$this->read_dependencies($depfile);
		$this->migrate_the_nomads();
		$rc_result = $this->recurse_candidates(0);
		if (!$rc_result )
		{
			echo ('#########   Recursion Failed   #########');
		}
		$this->read_objects($objfile);
	}

	function generate_output ()
	{
		//print_r ($this->sortedSrcList);
		//echo "1================================================================================================\n";
		//print_r ($this->orderedList);
		//echo "total orderedList = " . count ($this->orderedList) . "\n";

		$this->display_all_ada_files ();


		if (count ($this->missing_objs) > 0)
		{
			echo "\n\n################  OBJECTS NOT RECOGNIZED  ################\n";
			foreach ($this->missing_objs as $filename)
			{
				echo $filename . "\n";
			}
		}

		if (count ($this->unused_files) > 0)
		{
			echo "\n\n################  FILES NOT USED  ################\n";
			foreach ($this->unused_files as $filename)
			{
				echo $filename . "\n";
			}
		}


		$this->display_component_dependencies();
	}

	private function display_all_ada_files ()
	{
		$message = strtoupper("Generated File List -- Do Not Edit By Hand");
		echo "\n\n##############  $message  ##############\n\n";

		echo "set (compile_sequence\n";
		$counter =  0;

		# we haven't incorporated "[L]" local hints yet, so hard code to remote.
		foreach ($this->orderedList as $filename)
		{
			$cr = ($counter % 3 == 2) ? "\n" : '';
			$DBE = $this->dir_base_ext ($filename);
			$localremote = "R";
			$encoded = sprintf("%s:%s:%s/%s", $localremote, $DBE['extension'],
						$DBE['relpath'], $DBE['base']);
			$block = sprintf("     %-22s%s", $encoded, $cr);
			echo $block;
			$counter++;
		}
		if (($counter - 1) % 3 != 2) { echo "\n"; };
		echo ")";
		echo "\n\n##########################  END GENERATED BLOCK  ##########################\n\n";
	}

	private function display_component_dependencies ()
	{
		$message = strtoupper("Generated for " . $this->component . " -- Do Not Edit By Hand");
		echo "\n\n##############  $message  ##############\n\n";
		echo "set (" . $this->component . "_object_files\n";
		$counter =  0;
		$pattern = "%15s%s";
		foreach ($this->orderedList as $filename)
		{
			if (!in_array ($filename, $this->unused_files))
			{
				$DBE = $this->dir_base_ext ($filename);
				$ali = $DBE['base'] . '.o';
				$cr = ($counter % 5 == 4) ? "\n" : '';
				$block = sprintf($pattern, $ali, $cr);
				echo $block;
				$counter++;
			}
		}
		foreach ($this->missing_objs as $filename)
		{
			$DBE = $this->dir_base_ext ($filename);
			$ali = $DBE['base'] . '.o';
			$cr = ($counter % 5 == 4) ? "\n" : '';
			$block = sprintf($pattern, $ali, $cr);
			echo $block;
			$counter++;
		}
		if (($counter - 1) % 5 != 4) { echo "\n"; };
		echo ")";
		echo "\n\n##########################  END GENERATED BLOCK  ##########################\n\n";
	}

	private function set_as_built ($relpath, $basename)
	{
		$index = "$relpath/$basename.ads";
		$this->fileset_built [$index] = 1;
	}

	private function already_built ($relpath, $basename)
	{
		$index = "$relpath/$basename.ads";
		return $this->fileset_built [$index];
	}

	private function add_to_ordered_list ($filename)
	{
		$DBE = $this->dir_base_ext ($filename);
		$this->completeList [$filename] = 1;

		// if this is a spec file, but it has a body, we skip add it to the list.
		// When the body is compiled, the spec file will be grabbed automatically.
		if ($DBE['extension'] == 'ads')
		{
			if ($this->spec_has_body[$filename] == 1)
			{
				return;
			}
		}

		if (!in_array ($filename, $this->orderedList))
		{
			$this->orderedList[] = $filename;
		}
	}


	/**
	* @desc
	*  Part 1: We've identified all the .adb files that can be compiled separately, so build
	*  those first.
	*
	*  Part 2: All the specifications that don't have a body and also don't have a dedicated
	*  object target can be compiled first.  The members of this group have no dependencies.
	*
	* Part 3: We cycle through the list again and this time we grab .ads/.adb pairs that
	* don't have a dedicated object, and again assume no dependencies.
	*/
	private function migrate_the_nomads ()
	{
		foreach ($this->separate_comp as $filename)
		{
			$this->add_to_ordered_list($filename);
		}

		foreach ($this->spec_has_body as $filename => $has_body)
		{
			if ($has_body == 0)
			{
				$DBE = $this->dir_base_ext ($filename);
				$index = $DBE['relpath'] . '/' .  $DBE['base'] ;
				if (!isset ($this->objectList[$index]))
				{
					$this->add_to_ordered_list($filename);
				}
			}
		}
		foreach ($this->spec_has_body as $filename => $has_body)
		{
			if ($has_body == 1)
			{
				$DBE = $this->dir_base_ext ($filename);
				$index = $DBE['relpath'] . '/' .  $DBE['base'] ;

				if (!isset ($this->objectList[$index]))
				{
					$this->add_to_ordered_list($DBE['relpath'] . '/' .  $DBE['base'] . '.ads');
					$this->add_to_ordered_list($DBE['relpath'] . '/' .  $DBE['base'] . '.adb');
				}
			}
		}
	}

	/**
	* @desc This recursive function will activately remove items from the srcList
	* array and add them to class variable $sortedSrcList
	*
	* We advance if at least one "set as built" occurred in a pass.  The result is
	* true if all source files get built.
	*/


	/**
	* @desc This is an experimental version.  It's similar to the one commented out above, but
	* we only look at adb files.  By this time, all the nomads have migrated, so the only things
	* remaining are these paired adb / abs files.
	*/
	private function recurse_candidates ($level)
	{
		$complete_file_built = false;
		$keys = array_keys ($this->srcList);
		$checked = array ();
		foreach ($keys as $key)
		{
			$depends_built = true;
			$basis_found   = false;
			$dependencies = $this->srcList[$key][N1_DEPENDS];
			foreach ($dependencies as $dependency)
			{
				$DBE = $this->dir_base_ext ($dependency);
				if (($DBE['relpath'] == $this->srcList[$key][N1_RELPATH]) &&
					($DBE['base']    == $this->srcList[$key][N1_BASENAME]))
				{
					$basis_found = true;
					$checked[] = $dependency;
				}
				else
				{
					if (!isset ($this->completeList [$dependency]))
					{
						if ($DBE['extension'] == 'adb')
						{
							$depends_built = false;
						}
						$checked[] = $dependency;
					}
				}
			}
			if ($depends_built)
			{
				if ($basis_found)
				{
					$this->set_as_built($this->srcList[$key][N1_RELPATH], $this->srcList[$key][N1_BASENAME]);
				}
				foreach ($checked as $adb_filename)
				{
					$this->add_to_ordered_list($adb_filename);
				}
				$complete_file_built = true;
				$this->sortedSrcList[] = $this->srcList[$key];
				unset ($this->srcList[$key]);
			}
		}
		if (empty($this->srcList))
		{
			return true;
		}
		if (!$complete_file_built)
		{
			return false;
		}
		// If we get this far, we need to run through the list again.
		return $this->recurse_candidates($level+1);
	}




	private function match_spec_with_both_final ()
	{
		foreach (array_keys ($this->spec_has_body) as $index)
		{
			if (isset ($this->separate_comp [$index]))
			{
				$this->spec_has_body [$index] = 1;
				unset ($this->separate_comp [$index]);
			}
		}
	}

	private function match_spec_with_body ($filename)
	{
		$DBE = $this->dir_base_ext ($filename);

		if ($DBE['extension'] == 'ads')
		{
			$this->spec_has_body [$filename] = 0;
			$this->fileset_built [$filename] = 0;
		}
		else if ($DBE['extension'] == 'adb')
		{
			$index = $DBE['relpath'] . '/' . $DBE['base'] . '.ads';
			$this->separate_comp [$index] = $filename;
		}
	}

	private function dir_base_ext ($filename)
	{
		$pos = strrpos ($filename, '/');
		if ($pos === false)
		{
			$base_ext = $filename;
			$folder   = '';
		}
		else
		{
			$base_ext = substr ($filename, $pos + 1);
			$folder   = substr ($filename, 0, $pos);
		}
		$pos = strrpos ($base_ext, '.');
		if ($pos === false)
		{
			$base = $base_ext;
			$ext  = '';
		}
		else
		{
			$base = substr ($base_ext, 0, $pos);
			$ext  = substr ($base_ext, $pos + 1);
		}
		return array ('base' => $base, 'extension' => $ext, 'relpath' => $folder);
	}

	private function extract_dependency_info ($lineno, $line)
	{
		$pattern =  '/^(.+)\.o\s+:\s+(\S.+\S+)\s*/';    /* example: "ada/ada.o : ada/ada.ads ada/system.ads  " */
		$success = preg_match ( $pattern , $line , $matches);
		if ($success == 0)
		{
			die ("Line $lineno failed: $line");
		}
		else
		{
			$solospace = preg_replace ('/\s{2,}/', ' ', $matches[2]);
			$indyfiles = split(" ", $solospace);

			$DBE = $this->dir_base_ext ($matches[1]);
			$v0 = count ($indyfiles);
			$v1  = $DBE['base'];
			$v2  = $DBE['relpath'];
			$v3  = 0;
			$v4  = array ();

			foreach ($indyfiles as $filename)
			{
				$v4[] = $filename;
				$this->match_spec_with_body($filename);
			}
			$this->srcList[] = array ($v0, $v1, $v2, $v3, $v4);
			$this->objectList ["$v2/$v1"] = 1;
		}
	}

	private function read_dependencies ($inputFile)
	{
		$fgc = file_get_contents($inputFile);
		$slash = chr(92);
		$LF    = chr(10);
		$CR    = chr(13);
		$oneliners = str_replace(array ($slash.$LF, $slash.$CR.$LF), array ('',''), $fgc);
		$lineArray = explode("\n", $oneliners);
		foreach ($lineArray as $lineno => $line)
		{
			if (   ($line[0] != '#') && (trim($line) != ""))
			{
				$this->extract_dependency_info($lineno, $line);
			}
		}
		$this->match_spec_with_both_final();
		usort ($this->srcList, "depSort");
		foreach (array_keys($this->srcList) as $index)
		{
			$this->srcList[$index][N1_ORIGPOS] = $index;
		}
	}

	private function read_objects ($inputFile)
	{
		$fgc = file_get_contents($inputFile);
		$lineArray = explode("\n", $fgc);
		$this->unused_files = $this->orderedList;
		foreach ($lineArray as $lineno => $line)
		{
			$cleanline = trim ($line);
			if (!empty($cleanline))
			{
				$found = false;
				$DBE = $this->dir_base_ext ($cleanline);
				foreach (array_keys ($this->sortedSrcList) as $key)
				{
					if (($this->sortedSrcList[$key][N1_BASENAME] == $DBE['base']) &&
					     $this->sortedSrcList[$key][N1_RELPATH]  == $DBE['relpath'])
					{
						foreach ($this->sortedSrcList[$key][N1_DEPENDS] as $filename)
						{
							$index = array_search ($filename, $this->unused_files);
							if ($index !== false)
							{
								unset ($this->unused_files [$index]);
							}
						}
						$found = true;
						break;
					}
				}
				if (!$found)
				{
					$this->missing_objs[] = $cleanline;
				}
			}
		}

	}


}

$SortAda = new TSortAda ($depfile, $objfile, $inputs['objlist']);

if ($onweb)
{
	echo '<?xml version="1.0"?>'."\n";
	echo '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">' ."\n";
	echo '<html xmlns="http://www.w3.org/1999/xhtml">'."\n";
	echo '<head><title>Ada Objected Sorted By Dependency</title></head>'."\n";
	echo '<body><pre>'."\n";

	$SortAda->generate_output();

	echo '</pre></body></html>' . "\n";
}
else
{
	$SortAda->generate_output();
}


?>
