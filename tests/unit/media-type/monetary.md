Monetary properties for iCalendar
=================================

Abstract
--------
This document defines a monetary value type for calendar and a
corresponding price property.

Status of This Memo
-------------------
This value type and property only exists here to test the
extensibility of the calendar parsers and serializers. If this
property or value type are found to be useful, then this document
should be re-published as an actual RFC.

Copyright Notice
----------------

Table of Contents
-----------------

X. Introduction
---------------

X. Value Types
--------------

### Monetary

**Value Name:** MONETARY

#### Purpose
This value type is used to store monetary values.

#### Description
Monetary values can be used to embed ticket prices and similar. The value
consists of a currency indicator, which *should* be an ISO 4217 currency code,
and an amount in that currency. The amount is given as a float. However, care
should be taken during parsing that correct rounding is used.

#### iCalendar Format Definition

Assuming the values of `text` and `float` from RFC 5545.

    monetary = text ";" float

#### xCal XML Definition

    value-monetary = element monetary {
        element currency {
            xsd:string
        },
        element amount {
            xsd:float
        }
    }

#### Example:

iCalendar

    PRICE:SEK;10.00
    
xCal
    
    <price>
      <monetary>
        <currency>SEK</currency>
        <amount>10.00</amount>
      </monetary>
    </price>

jCal
    
    ["price", {}, "monetary", ["SEK", 10.00]]

X. Properties
-------------

### Price
