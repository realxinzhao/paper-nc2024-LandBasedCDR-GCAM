/*! 
* \file summary.cpp
* \ingroup CIAM
* \brief summary class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include "summary.h"
#include "World.h"
#include "Scenario.h"
#include <vector>

using namespace std;

extern Scenario* scenario;

//! Default constructor
Summary::Summary() {
}

void Summary::initfuelcons( const string& fname, const double value ){

	fuelcons[ fname ] += value;
	fuelcons[ "zTotal" ] += value;
}

void Summary::initpeprod( const string& fname, const double value ){

	peprod[ fname ] += value;
	peprod[ "zTotal" ] += value;
}

void Summary::initemission( const string& ghgname, const double value)
{
	emission[ ghgname ] += value;
	emission[ "zTotal" ] += value;
}

map<string, double> Summary::getfuelcons() const {
	
	return fuelcons;
}

map<string, double> Summary::getpecons() const {
	
	return pecons;
}

map<string, double> Summary::getpetrade() const {
	return petrade;
}

map<string, double> Summary::getemission() const {
	
	return emission;
}

map<string, double> Summary::getemfuelmap() const {
	
	return emissfuel;
}

map<string, double> Summary::getemindmap() const {

	return emissind;
}

//! return map of sequestered amount of emissions
map<string, double> Summary::getSequesteredAmountMap() const {

	return sequesteredAmount;
}

//! Add the passed fuelmap to the summary fuelinfo map 
/* The consumption values in the fuelinfo map that is passed are added 
to the summary object maps fuelcons and pecons.

The iterator fmap is used to traverse the fuelinfo map.
*/ 
void Summary::updatefuelcons( const map<string, double>& fuelinfo ) {
	
	string str;
	typedef map<string,double>:: const_iterator CI;
    CI fmap;
        
	// map all primary and secondary fuel consumption
	for (fmap=fuelinfo.begin(); fmap!=fuelinfo.end(); ++fmap) {	// iterate to one less than the end
		fuelcons[fmap->first] += fmap->second; // Add values from the passed map to fuelcons
        // Don't need a zTotal b/c the fuels are uncomparable. 
	}

	// map primary energy consumption only.
   const vector<string> primaryFuelList = scenario->getWorld()->getPrimaryFuelList();
   
   for( vector<string>::const_iterator fuelIter = primaryFuelList.begin(); fuelIter != primaryFuelList.end(); fuelIter++ ) {
	   fmap=fuelinfo.find( *fuelIter );
	   if(fmap!=fuelinfo.end()) {
		   pecons[fmap->first] += fmap->second;
		   pecons["zTotal"] += fmap->second;
	   }
   }
}

void Summary::updatepetrade() {

	// map all primary and secondary fuel consumption
	for ( map<string,double>::const_iterator fmap = peprod.begin(); fmap != peprod.end(); ++fmap ) {
		petrade[ fmap->first ] = peprod[ fmap->first ] - pecons[ fmap->first ];
	}
}

void Summary::updateemiss( const map<string, double>& ghginfo ) {
	
	// map all primary and secondary fuel consumption
	for ( map<string,double>::const_iterator fmap = ghginfo.begin(); fmap != ghginfo.end(); ++fmap){
		emission[ fmap->first ] += fmap->second;
	}
}

void Summary::updateemfuelmap( const map<string, double>& ghginfo ) {

	// map all primary and secondary fuel consumption
	for ( map<string,double>::const_iterator fmap = ghginfo.begin(); fmap != ghginfo.end(); ++fmap ) {
		emissfuel[ fmap->first ] += fmap->second;
	}
}

void Summary::updateemindmap( const map<string, double>& ghginfo ) {

	// map all primary and secondary fuel consumption
	for ( map<string,double>::const_iterator fmap = ghginfo.begin(); fmap != ghginfo.end(); ++fmap ) {
		emissind[ fmap->first ] += fmap->second;
	}
}

//! update the map of sequestered amount of emissions
void Summary::updateSequesteredAmountMap( const map<string, double>& ghginfo ) {

	// map sequestered amount of CO2 for secondary fuels and zTotal
	for ( map<string,double>::const_iterator fmap = ghginfo.begin(); fmap != ghginfo.end(); ++fmap ) {
		sequesteredAmount[ fmap->first ] += fmap->second;
	}
}

void Summary::clearfuelcons() {
	
	fuelcons.clear();
	pecons.clear();
}

void Summary::clearpeprod() {

	peprod.clear();
	petrade.clear();
}

void Summary::clearemiss() {

	emission.clear();
}

void Summary::clearemfuelmap() {
	
	emissfuel.clear();
}

void Summary::clearemindmap() {
	emissind.clear();
}

//! clear out map of sequestered amount
void Summary::clearSequesteredAmountMap() {
	sequesteredAmount.clear();
}

double Summary::get_fmap_second( const string& name ) const {
	return ( fuelcons.find( name ) )->second;
}

double Summary::get_pemap_second( const string& name ) const {
	return ( pecons.find( name ) )->second;
}

double Summary::get_petrmap_second( const string& name ) const {
	return ( petrade.find( name ) )->second;
}

double Summary::get_peprodmap_second( const string& name ) const {
	return ( peprod.find( name ) )->second;
}

double Summary::get_emissmap_second( const string& name ) const {
	return ( emission.find( name ) )->second;
}

//! return the sequestered amount which is second part of the map
double Summary::getSequesteredAmount( const string& name ) const {
	return ( sequesteredAmount.find( name ) )->second;
}

double Summary::get_emissfuelmap_second( const string& name ) const {
	return ( emissfuel.find( name ) )->second;
}

double Summary::get_emindmap_second( const string& name ) const {
	return ( emissind.find( name ) )->second;
}

