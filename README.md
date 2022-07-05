# TLDR #

Chimera is an Open Source (MIT License) library for modern Delphi releases.  It includes an extremely fast and standard JSON implements as well as utilities useful when working with JSON.

# Core Features #

## JSON ##

Chimeara provides a simplified JSON library.  To initialize a new JSON object, simply call:
	
```
  var jso := TJson.New;
```
	
or alternatively to load JSON from a string in memory you can use: 

```
  var jso := TJSON.From('{"item":"soda", "cost":4.00};
```
	
In both cases, the type of the jso variable is IJSONObject.  You never have to worry about freeing the json object making memory management easly.
	
To access a property of that object you use the property of the type of the object you are looking for:
	
```
  ShowMessage(jso.Strings['item']+' is '+CurrToStr(jso.numbers['cost']));
```

... resulting in a message that says "soda is $4.00".

Type properties can easily be set as well just as they are read:

```
  jso.Numbers['cost'] := qry.FieldByName('cost').AsFloat;
```

To initialize a new object property, just assign a new empty object to it:

```
  jso.Objects['attributes'] := TJSON.New;
```
... and then you can set that objects properties accordingly:

```
  jso.Objects['attributes'].Strings['brand'] := 'Pepsi';
```

Arrays can be added using the TJSONArray helper class:

```
  jso.Arrays['sales'] := TJSONArray.New;
  jso.Arrays['sales'].Add(jsoSale); // where jsoSale is another instace of a IJSONObject holding sales data
```
	
Arrays can easily be iterated with the Each property:

```
  jso.Arrays['sales'].Each(
    procedure(Sale : IJSONObject)
	begin
	  // Do something with the Sale object here
	end
  );
```

JSON can easily be imported and exported to a text file or other stream using the SaveTo and LoadFrom methods:
```
  jso.LoadFromFile('sales.json');
  jso.Dates['updated'] := Now;
  jso.SaveToFile('sales.json');
```

That example also shows off a helper accessor called Dates which automatically converts Delphi TDateTimes to and from ISO8601 format in a string property.  There are several other accessor helpers which map Delphi types into the base JSON types:

| Name | Type | Description |
| --- | --- | --- |
| GUIDs[] | TGuid | converts a TGUID to and from a string representation of a guid.|
| Bytes[] | TArray\<Byte\> | converts a byte array to a base64 encoded binary string.|
| Dates[] | TDateTime | converts a TDateTime to and from an ISO8601 formatted string|
| LocalDates[] | TDateTime | converts a TDateTime to and from an ISO8601 formatted string converting from and to UTC.|
| IntDates[] | TDateTime | converts a TDateTime to and from an integer value representing the number of seconds since January 1st 1970|
| Items[const idx : integer] | Variant | converts a Delphi variant to and from the closest JSON type that is applicable.|

And the standard json types are represented with the following accessors:

| Name | Type |
| --- | --- |
| Strings[] | string
| Numbers[] | Double
| Integers[] | Int64
| Booleans[] | Boolean 
| Objects[] | IJSONObject
| Arrays[] | IJSONArray

Since not all properties or array items may be the same type, Chimera gives you an accessor specifically to figure out what is in your JSON:

| Name | Type | Description |
| --- | --- | --- |
| Types[] | TJSONValueType | a Value that is one of the following `TJSONValueType = (&string, number, &array, &object, boolean, null, code);` |

