### 0.3.4 August 5th 2015
* More robust textual date parsing
* Update DotNetZip to 1.9.5

### 0.3.3 July 26th 2015
* Update parsing to support Common Sense data types
* Performance optimizations in iron man parsing
* Support higher precision numbers

### 0.3.2 - June 23rd 2015
* Fix interpretation of empty '{}' in binary format from arrays to objects
* Fix int vs. uint binary type detection
* Interpret binary dates of `1.1.1` correctly

### 0.3.1 - June 21st 2015
* Swap zip implementation for one that works

### 0.3.0 - June 19th 2015
* Add ability to parse ironman saves
* Add ability to parse compressed saves
* Rename static `ParaValue` functions to highlight between plain text and binary
  parsing
* Add basic `ToString` functionality to `ParaValue`

### 0.2.1
* Cleaned up some of the code
* Add rudimentary saving

### 0.2.0
* Rename `DoubleParse` file to `Utils`
* Rename `DoubleParse.tryParse` to `Utils.tryDoubleParse`
* Add `tryDateParse` to `Utils`
* Add xDefault functions to easy handling of `None` cases

### 0.1.4
* Fix parsing empty objects at the end of another object

### 0.1.3
* Add `findOptional` to API

### 0.1.2
* Fix the implementation of `tryFind` to follow documented API

### 0.1.1 - A Proper Release
* Fix the image url link on Nuget to point to this project's logo
* Fix author's name and email in metadata

### 0.1.0 - Initial Release
* Initial Release
