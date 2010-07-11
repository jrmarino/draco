<?php
/**
* @desc This script will put all the draco source files in their proper places.
* It requires an input "gccroot" which normally follows the pattern:
*     /some-path/gcc-4.6-20100626
* It also requires an input "location" which is the directory where the source
* files will be placed.  Something like /path-to-location/draco-{version}/
* with subdirs of gcc_remnants, gcc_interface, ada, driver, etc.
* The "repos" input is the full path to the local draco repository
*
* This script is only intended for command line use
*/

$arguments = isset($argv) ? $argv : array();
$numArgs = count($arguments);
for ($i = 1; $i < $numArgs; $i++)
{
   parse_str($arguments[$i]);
}

$inputs = array (
   'gccroot'  => null,
   'location' => null,
   'repos'    => null
);
$options = compact('gccroot','location','repos');

foreach (array_keys ($inputs) as $index)
{
   if (isset($options[$index]))
   {
      if (!is_dir ($options[$index]))
      {
         die ($index . ': There is no directory called "' . $options[$index] . '"' . "\n");
      }
      $inputs[$index] = realpath( $options[$index] );
   }
   else
   {
      die ("The required input '$index' was not provided.  Aborting....\n");
   }
}


class TDracoSource
{
   private $dir_gccroot;
   private $dir_location;
   private $dir_repos;
   private $dir_src_location;
   private $draco_version;

   function __construct ($gccroot, $location, $repos)
   {
      $this->dir_gccroot  = $gccroot;
      $this->dir_location = $location;
      $this->dir_repos    = $repos;

      $this->determine_draco_version();
      $this->create_structure();
      copy (
         $this->dir_repos        . '/draco_trunk/CMakeLists.txt',
         $this->dir_src_location . '/CMakeLists.txt'
      );
      $this->copy_directory_contents(
         $this->dir_repos        . '/trunk/gcc/ada/gcc-interface',
         $this->dir_src_location . '/gcc_interface'
      );
      $this->copy_directory_contents(
         $this->dir_repos        . '/trunk/gcc/ada',
         $this->dir_src_location . '/ada'
      );
      $this->copy_directory_contents(
         $this->dir_repos        . '/draco_trunk/driver',
         $this->dir_src_location . '/driver'
      );
      $this->copy_directory_contents(
         $this->dir_repos        . '/draco_trunk/constructs',
         $this->dir_src_location . '/constructs'
      );
      $this->copy_directory_contents(
         $this->dir_gccroot      . '/include',
         $this->dir_src_location . '/gcc_remnants/include'
      );
      $this->copy_directory_contents(
         $this->dir_gccroot      . '/libcpp',
         $this->dir_src_location . '/gcc_remnants/libcpp'
      );
      $this->copy_directory_contents(
         $this->dir_gccroot      . '/libdecnumber',
         $this->dir_src_location . '/gcc_remnants/libdecnumber'
      );
      $this->copy_directory_contents(
         $this->dir_gccroot      . '/libiberty',
         $this->dir_src_location . '/gcc_remnants/libiberty'
      );
      $this->copy_directory_contents(
         $this->dir_gccroot      . '/gcc',
         $this->dir_src_location . '/gcc_remnants/gcc'
      );
      $this->copy_directory_contents(
         $this->dir_gccroot      . '/gcc/c-family',
         $this->dir_src_location . '/gcc_remnants/gcc/c-family'
      );
      $this->copy_directory_contents(
         $this->dir_gccroot      . '/gcc/config',
         $this->dir_src_location . '/gcc_remnants/gcc/config'
      );
      $this->copy_directory_contents(
         $this->dir_gccroot      . '/gcc/config/arm',
         $this->dir_src_location . '/gcc_remnants/gcc/config/arm'
      );
      $this->copy_directory_contents(
         $this->dir_gccroot      . '/gcc/config/i386',
         $this->dir_src_location . '/gcc_remnants/gcc/config/i386'
      );
      $this->copy_directory_contents(
         $this->dir_gccroot      . '/gcc/config/sparc',
         $this->dir_src_location . '/gcc_remnants/gcc/config/sparc'
      );
      /* ####### HEAD: disabled for now #######

      $this->copy_directory_contents(
         $this->dir_gccroot      . '/gcc/config/alpha',
         $this->dir_src_location . '/gcc_remnants/gcc/config/alpha'
      );
      $this->copy_directory_contents(
         $this->dir_gccroot      . '/gcc/config/ia64',
         $this->dir_src_location . '/gcc_remnants/gcc/config/ia64'
      );
      $this->copy_directory_contents(
         $this->dir_gccroot      . '/gcc/config/mips',
         $this->dir_src_location . '/gcc_remnants/gcc/config/mips'
      );
      $this->copy_directory_contents(
         $this->dir_gccroot      . '/gcc/config/rs6000',
         $this->dir_src_location . '/gcc_remnants/gcc/config/rs6000'
      );

         ####### TAIL: disabled for now #######  */

   }

   /**
   * @desc This function does not copy subdirectories.
   */
   private function copy_directory_contents ($fromdir, $todir)
   {
      $filelist = scandir($fromdir);
      $listlen = count($filelist);
      for ($x = 2; $x < $listlen; $x++)
      {
         $suffix = '/' . $filelist[$x];
         if (!is_dir($fromdir . $suffix))
         {
            copy ($fromdir . $suffix, $todir .$suffix);
         }
      }
   }

   private function create_structure ()
   {
      $topdir = $this->dir_location . '/DRACO-' . $this->draco_version;
      if (file_exists($topdir))
      {
         $found = true;
         $instance = 0;
         $proposed_name = 'tbd';
         while ($found)
         {
            $instance++;
            $suffix = sprintf('.old.%02d', $instance);
            $proposed_name = $topdir . $suffix;
            $found = file_exists ($proposed_name);
         }
         rename($topdir, $proposed_name);
      }
      mkdir ($topdir, 0777);
      mkdir ($topdir . '/gcc_remnants', 0777);
      mkdir ($topdir . '/gcc_remnants/include', 0777);
      mkdir ($topdir . '/gcc_remnants/libcpp', 0777);
      mkdir ($topdir . '/gcc_remnants/libdecnumber', 0777);
      mkdir ($topdir . '/gcc_remnants/libiberty', 0777);
      mkdir ($topdir . '/gcc_remnants/gcc', 0777);
      mkdir ($topdir . '/gcc_remnants/gcc/c-family', 0777);
      mkdir ($topdir . '/gcc_remnants/gcc/config', 0777);
      mkdir ($topdir . '/gcc_remnants/gcc/config/arm', 0777);
      mkdir ($topdir . '/gcc_remnants/gcc/config/i386', 0777);
      mkdir ($topdir . '/gcc_remnants/gcc/config/sparc', 0777);
      /* ####### HEAD: disabled for now #######
      mkdir ($topdir . '/gcc_remnants/gcc/config/alpha', 0777);
      mkdir ($topdir . '/gcc_remnants/gcc/config/ia64', 0777);
      mkdir ($topdir . '/gcc_remnants/gcc/config/mips', 0777);
      mkdir ($topdir . '/gcc_remnants/gcc/config/rs6000', 0777);
        ####### TAIL: disabled for now #######  */
      mkdir ($topdir . '/gcc_interface', 0777);
      mkdir ($topdir . '/driver', 0777);
      mkdir ($topdir . '/ada', 0777);
      mkdir ($topdir . '/constructs', 0777);
      $this->dir_src_location = $topdir;
   }

   /**
   * @desc This function does two things:
   * 1) It defines the draco version number, e.g. DRACO-2010-Q3
   * 2) It checks that CMakelists.txt exists, which means the dir_repos directory input
   *    is valid and doesn't need to be checked again.
   */
   private function determine_draco_version ()
   {
      $master_cmake_script = $this->dir_repos . '/draco_trunk/CMakeLists.txt';
      if (!file_exists($master_cmake_script))
      {
         die ('The CMakelists.txt file does not exist, so check the value of the "repos" option.'."\n");
      }
      $fgc = file_get_contents($master_cmake_script);
      $result = preg_match('/set\s\(DRACO_VERSION\s+\"(.+)\"/', $fgc, $matches);
      if (!$result)
      {
         die ("The DRACO_VERSION variable was not found in the CMakeLists.txt file.\n");
      }
      else
      {
         $this->draco_version = $matches[1];
      }
   }
}

new TDracoSource($inputs['gccroot'], $inputs['location'], $inputs['repos']);
?>
