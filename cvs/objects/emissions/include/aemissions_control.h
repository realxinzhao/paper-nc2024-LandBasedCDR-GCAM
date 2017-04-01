#ifndef _AEMISSIONS_CONTROL_H_
#define _AEMISSIONS_CONTROL_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/



/*! 
* \file aemissions_control.h
* \ingroup Objects
* \brief The AEmissionsControl class header file.
* \author Kate Calvin
*/

#include <xercesc/dom/DOMNode.hpp>
#include <string>
#include "util/base/include/iround_trippable.h"

// Forward declarations
class GDP;
class IInfo;
class NonCO2Emissions;

/*! 
 * \ingroup Objects
 * \brief The AEmissionsControl class describes a emissions control option.
 * \details The AEmissionsControl class describes a means of reducing emissions.
 * \author Kate Calvin
 */
class AEmissionsControl: public IRoundTrippable {
public:
    //! Virtual Destructor.
    virtual ~AEmissionsControl();
    //! Clone operator.
    virtual AEmissionsControl* clone() const = 0;
    
    void XMLParse( const xercesc::DOMNode* aNode );
    void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;
    void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;
    static const std::string& getXMLNameStatic();
    
    double getEmissionsReduction( const std::string& aRegionName, const int aPeriod, const GDP* aGDP );

    virtual const std::string& getName() const;

    /*!
     * \brief Complete the initialization of the ghg object.
     * \note This routine is only called once per model run
     * \param aRegionName Region name.
     * \param aSectorName Sector name, also the name of the product.
     * \param aTechInfo Technology information object.
     * \author Pralit Patel
     * \warning Markets are not necessarily set when completeInit is called
     */
    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const IInfo* aTechIInfo ) = 0;

    /*!
     * \brief Perform initializations that only need to be done once per period.
     * \param aRegionName Region name.
     * \param aTechInfo The local information object.
     * \param aParentGHG The NonCO2Emissions that contains this object.
     * \param aPeriod Model period.
     */
    virtual void initCalc( const std::string& aRegionName,
                           const IInfo* aTechInfo,
                           const NonCO2Emissions* aParentGHG,
                           const int aPeriod ) = 0;

protected:

    AEmissionsControl();
    AEmissionsControl( const AEmissionsControl& aOther );
    AEmissionsControl& operator=( const AEmissionsControl& aOther );

    /*!
     * \brief Get the XML node name for output to XML.
     * \details This public function accesses the private constant string,
     *          XML_NAME. This way the tag is always consistent for both read-in
     *          and output and can be easily changed. This function may be
     *          virtual to be overridden by derived class pointers.
     * \author Jim Naslund
     * \return The constant XML_NAME.
     */
    virtual const std::string& getXMLName() const = 0;

    /*!
     * \brief Parses any child nodes specific to derived classes
     * \details Method parses any input data from child nodes that are specific
     *          to the classes derived from this class.
     * \author Josh Lurz, Steve Smith
     * \param aNodeName name of current node
     * \param aCurrNode pointer to the current node in the XML input tree
     * \return Whether any node was parsed.
     */
    virtual bool XMLDerivedClassParse( const std::string& aNodeName, const xercesc::DOMNode* aCurrNode ) = 0;
    
    /*!
     * \brief XML output stream for derived classes
     * \details Function writes output due to any variables specific to derived
     *          classes to XML
     * \author Jim Naslund
     * \param aOut reference to the output stream
     * \param aTabs A tabs object responsible for printing the correct number of
     *        tabs. 
     */
    virtual void toInputXMLDerived( std::ostream& aOut, Tabs* aTabs ) const = 0;
    
    /*!
     * \brief XML debug output stream for derived classes
     * \details Function writes output due to any variables specific to derived
     *          classes to XML
     * \author Jim Naslund
     * \param aPeriod The model period.
     * \param aOut reference to the output stream
     * \param aTabs A tabs object responsible for printing the correct number of
     *        tabs. 
     */
    virtual void toDebugXMLDerived( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const = 0;

    virtual void calcEmissionsReduction( const std::string& aRegionName, const int aPeriod, const GDP* aGDP ) = 0;

    void setEmissionsReduction( double aReduction );
    
private:
    //! Name of the reduction so that users can have multiple emissions reductions
    std::string mName;
    
    //! Reduction (usually calculated)
    double mReduction;

    void copy( const AEmissionsControl& aOther );

};

#endif // _AEMISSIONS_CONTROL_H_
