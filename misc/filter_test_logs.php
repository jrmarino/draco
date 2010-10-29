<?php
/**
* @desc This script loads the acats.sum and gnat.sum files, filters out the
* passed tests and returns the concatenated result in a file called
* snapshot_{year}_{month}_{day}.results
*
* year is 4 digits.  Month and day are 2 digits with leading zeros.
* However, this is not enforced, but only used to create the resultant file.
*
* If the tool was called by a webserver, the file isn't created but rather
* the contents are sent to the webserver as an html page.
*/

$arguments = isset($argv) ? $argv : array();
$numArgs = count($arguments);
for ($i = 1; $i < $numArgs; $i++)
{
	parse_str($arguments[$i]);
}

$inputs = array ('snapshot' => null);
$options = compact('snapshot');
$onweb = false;

foreach (array_keys ($inputs) as $index)
{
	if (isset($options[$index]))
	{
		$inputs[$index] = $options['snapshot'];
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
$acatfile = "$workdir/acats.sum";
$gnatfile = "$workdir/gnat.sum";

if (!file_exists($acatfile))
{
	die ("The file '$acatfile' does not exist.  Aborting...");
}
else if (!file_exists($gnatfile))
{
	die ("The file '$gnatfile' does not exist.  Aborting...");
}


class TFilterLogs
{
	private $acat_file_contents;
	private $gnat_file_contents;
	private $resultant_contents;
	private $snapshot_identifier;

	public function __construct ($acatfile, $gnatfile, $snapshot)
	{
		$this->snapshot_identifier = $snapshot;
		$this->acat_file_contents = file_get_contents ($acatfile);
		$this->gnat_file_contents = file_get_contents ($gnatfile);
		$this->resultant_contents = $this->filter_acat_log() . $this->filter_gnat_log();
	}

	private function filter_acat_log ()
	{
		$pattern = "/^PASS:\t[[:digit:][:lower:]]{6}/";

		$result = "";
		$lines = explode ("\n", $this->acat_file_contents);
		foreach ($lines as $line)
		{
			if (!preg_match($pattern, $line))
			{
				$result .= $line . "\n";
			}
		}
		return $result;
	}

	private function filter_gnat_log ()
	{
		$pattern = "/^(PASS|XFAIL):\s.+/";

		$result = "";
		$lines = explode ("\n", $this->gnat_file_contents);
		foreach ($lines as $line)
		{
			if (!preg_match($pattern, $line))
			{
				$result .= $line . "\n";
			}
		}
		return $result;
	}

	public function generate_file ()
	{
		$workdir = dirname (__FILE__);
		$filename = $workdir . '/snapshot_' . $this->snapshot_identifier . '.results';
		file_put_contents($filename, $this->resultant_contents);
	}

	public function generate_output ()
	{
		echo $this->resultant_contents;
	}
}


$FilterLogs = new TFilterLogs ($acatfile, $gnatfile, $inputs['snapshot']);

if ($onweb)
{
	echo '<?xml version="1.0"?>'."\n";
	echo '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">' ."\n";
	echo '<html xmlns="http://www.w3.org/1999/xhtml">'."\n";
	echo '<head><title>Filtered Test Results for ' . $inputs['snapshot'] . 'Snapshot</title></head>'."\n";
	echo '<body><pre>'."\n";

	$FilterLogs->generate_output();

	echo '</pre></body></html>' . "\n";
}
else
{
	$FilterLogs->generate_file();
}
?>
