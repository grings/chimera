# JSON Chimera for Delphi #

## TLDR ##

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
| Times[] | TDateTime | converts a TDateTime to and from a simple time string in the format of `h:mm am/pm` |
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

Arrays can be added using the TJSONArray helper class:

```
  jso.Arrays['sales'] := TJSONArray.New;
  jso.Arrays['sales'].Add(jsoSale); // where jsoSale is another instace of a IJSONObject holding sales data
```

To easily move a Delphi Array into a JSON array, you can use:

```
  jso.Arrays['strings'] := TJSONArray.From<TArray<string>>(['first','second']);
  
```
	
You can easily convert a JSON Array to a delphi array using the helper methods:
```
    function AsArrayOfStrings : TArray<string>; overload;
    function AsArrayOfGUIDs : TArray<TGuid>; overload;
    function AsArrayOfDateTimes : TArray<TDateTime>; overload;
    function AsArrayOfNumbers : TArray<Double>; overload;
    function AsArrayOfIntegers : TArray<Int64>; overload;
    function AsArrayOfBooleans : TArray<Boolean>; overload;
    function AsArrayOfObjects : TArray<IJSONObject>; overload;
    function AsArrayOfArrays : TArray<IJSONArray>; overload;

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

## Object and Dataset Serialization / Deserialization ##

Chimera adds some handy Object helpers via the chimera.json.helpers.*.pas units. 

You can easily Serialize and Deserialize a TObject descendant by using the new TObject.AsJSONObject property. The following code will take a TEdit named LoginEdit, serialize the object to JSON, alter a property and deserialize back to LoginEdit:

```
  var jso := LoginEdit.AsJSONOject;
  jso.Strings['Text'] := 'changed text in json';
  LoginEdit.AsJSONObject := jso;  
```

If you'd rather send the json representation of that object straight to text, you can do so with the AsJSON property:

```
  Memo1.Lines.Text := LoginEdit.AsJSON;
```

In addition, sometimes it's useful to store complex data or state with an object.  This is now possible using the TObject.TagJSON property.

```
  TreeNode1.TagJSON := TJSON.FromFile('FirstNode.json');
```

Likewise, datasets can be easily exported AsJSON property

```
  MyTable.Open;
  Send(MyTable.AsJSON);
```

In addition, datasets can be updated by calling UpdateFields with a JSON object repreenting the data to update in the current row.

```  
  ReceiveRecordToUpdateAsJSON(ID, s);
  MyTable.Locate('ID', ID);
  MyTable.UpdateFields(TJSON.From(s));
```


## JWT / JWK

Java Web Tokens and Java Web Keys have been a standard part of several authentication and verification schemes in today's web world.  Instantiating a JWT or JWK is very easy using the chimera.json.jwt.pas and chimera.json.jtk.pas units

JWT:

```
  var jwt := TJWT.New;
  jwt.ValidateHS256(sJWTFromWeb, sKnownSecret); // Raises if invalid
  
  if not jwt.TryValidateHS256(sJWTFromWeb, sKnownSecret) then // Does not raise if invalid
    // Do something
    
  var myjwt := TJWTNew;
  Send(myjwt.SignHS224('MySecret');
  
```

JWK:

```
  var jwk := TJWK.New;
  jwk.Add('param','value');
  
  var jwkset := TJWKSet.New;
  jwkset.Add(jwk);
  send(jwkset.AsJSON);
  
  // or more compactly...
  var jwkset2 := TJWKSet.New;
  jwkset2.Add('param','value);
  send(jwkset2.AsJSON);
```

## Pubsub ##

The publish / subscribe pattern is very popular in today's web development and design.  Chimera supports working with pubsub in a couple very useful ways.

- An Internal PubSub server implementation can be found in the chimera.pubsub.server unit.  In addition a WebBroker Producer component version is provided int he chimera.pubsub.webbroker unit.
- A Pubsub client implmentation that supports the internal pubsub server can be found in the chimera.pubsub.client unit.
- A Pubsub client implementation that is compatible with the Comet/J protocol as used in Faye's Ruby and Node Server in use in hundreds of thousands of implementations worldwide.