# JSON Chimera for Delphi #

## TLDR ##

Chimera is an Open Source (MIT License) library for modern Delphi releases.  It includes an extremely fast and standard JSON implements as well as utilities useful when working with JSON.

## Installation ##

### Option A — DPM (recommended for application projects)

[DPM](https://docs.delphi.dev/) (Delphi Package Manager) installs Chimera as a **source-only**
package. DPM downloads the sources into your local package cache and adds them to the
project search path via `$(DPMSearch)`. Nothing is registered in the IDE and no `.bpl` is
required — units compile into your app like any other source.

| | |
|---|---|
| **Package id** | `sivv.chimera` |
| **Feed** | [delphi.dev](https://delphi.dev/) public gallery (registered by default on first DPM run) |
| **Delphi versions** | 10.0 Seattle through 13.0 Florence |
| **Installed units** | JSON, JWT/JWK, pub/sub, storage, and utility units under `src\` in the package cache |

#### 1. One-time setup

1. Install the [DPM client](https://docs.delphi.dev/getting-started/installing.html)
   (command-line tool and IDE plugin). Leave **Add to PATH** enabled.
2. Confirm a gallery source is configured:

   ```text
   dpm sources List
   ```

   You should see a source pointing at `https://delphi.dev/...` (often named `DPM` or
   `delphi.dev`). If not:

   ```text
   dpm sources Add "-name=delphi.dev" "-source=https://delphi.dev/api/v2/index.json" -type=DPMServer
   dpm sources Enable "-name=delphi.dev"
   ```

#### 2. Add the package to your project

**From the IDE:** open your `.dproj`, right-click the project → **Manage DPM
Packages**, search for `sivv.chimera`, and install.

**From the command line** (run from any folder):

```text
dpm install sivv.chimera C:\path\to\YourProject.dproj
```

Pin a specific release:

```text
dpm install sivv.chimera -version=1.0.0 C:\path\to\YourProject.dproj
```

Limit platforms if needed (defaults to all platforms your project targets):

```text
dpm install sivv.chimera -platforms=Win32,Win64 C:\path\to\YourProject.dproj
```

#### 3. Use it in code

```pascal
uses chimera.json;              // core JSON library
uses chimera.json.helpers;      // TObject / dataset serialization helpers
uses chimera.json.jwt;          // JWT support
uses chimera.json.jcs;          // RFC 8785 JSON Canonicalization Scheme (JCS)
uses chimera.pubsub;            // internal pub/sub server
uses chimera.bayeux.client;     // Comet/Bayeux (Faye-compatible) client
uses chimera.storage;           // storage engine
uses chimera.utility;           // general utilities
```

Some units require optional Delphi packages (for example Indy units for
`chimera.pubsub.client.idhttp`, `chimera.pubsub.server.idhttp`, and
`chimera.storage.local`, or `inet` for `chimera.pubsub.producer`). Add those
dependencies only when you use the corresponding units.

#### 4. What DPM changes in your `.dproj`

After install, commit the DPM-related edits to version control:

- **`PackageReference`** elements at the bottom of the `.dproj` (package id + version).
- **`$(DPMSearch)`** added to `DCC_UnitSearchPath` for each installed platform.

Other developers (and CI) do **not** need a copy of this repo. After cloning your
project:

```text
dpm restore C:\path\to\YourProject.dproj
```

That downloads the referenced package version into `%AppData%\.dpm\` and wires up the
same search paths.

#### 5. Upgrade or remove

```text
dpm install sivv.chimera -version=1.0.1 -upgrade C:\path\to\YourProject.dproj
```

Use the IDE package manager to uninstall, or edit the `PackageReference` entries and
run `dpm restore`.

#### Local / private feed

If your team mirrors packages internally, register that folder or server as an
additional DPM source and pass `-source=YourSourceName` on `install` / `restore`. The
package spec lives at
[`sivv.chimera.dspec.yaml`](sivv.chimera.dspec.yaml).

### Option B — Manual (clone / submodule)

Add this repo (or copies of the `json`, `pubsub`, `storage`, `utility`, and `common`
folders) to your project's search path. Demo and test projects in this repo use relative
paths so contributors can build without DPM.

### Maintainers — pack and publish

Tag the release first (`git tag release/1.0.0`), then:

```powershell
$env:DPM_API_KEY = 'your-api-key'
.\scripts\publish-dpm.ps1
```

The script reads the version from the `release/*` tag at HEAD (or the latest
`release/*` tag), packs into `dist\dpm`, and pushes to the `delphi.dev` gallery.
Pack only: `.\scripts\publish-dpm.ps1 -PackOnly` · Push only: `-PushOnly` · See
[`scripts/publish-dpm.ps1`](scripts/publish-dpm.ps1).

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
| Integers[] | Int64 | converts a delphi integer to and from a number value in cases where the value is expected to always be an non-floating point value. |
| Items[const idx : integer] | Variant | converts a Delphi variant to and from the closest JSON type that is applicable.|

And the standard json types are represented with the following accessors:

| Name | Type |
| --- | --- |
| Strings[] | string |
| Numbers[] | Double |
| Booleans[] | Boolean | 
| Objects[] | IJSONObject |
| Arrays[] | IJSONArray |

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


## JSON Canonicalization (JCS)

Chimera supports [RFC 8785](https://www.rfc-editor.org/info/rfc8785) JSON Canonicalization Scheme (JCS) as an **opt-in** serializer for deterministic, hashable JSON output. Default `AsJSON` formatting is unchanged.

Use `AsJCS` when you need a canonical JSON text representation, and `AsJCSBytes` when you need the UTF-8 octets for hashing or signing:

```
  var jso := JSON('{"b":2,"a":1}');
  WriteLn(jso.AsJCS);       // {"a":1,"b":2}
  HashBytes(jso.AsJCSBytes);
```

You can also canonicalize JSON text directly:

```
  WriteLn(JCS('{"b":2,"a":1}'));
```

`AsSHA1` and `SameAs` use Chimera's standard JSON serializer, not JCS. For interoperable digests (for example across Node, Java, or Go stacks), use `AsJCSBytes` with your preferred hash function instead.

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
  
  // JWK thumbprint (RFC 7638) over JCS-canonical member data
  WriteLn(jwk.Thumbprint);
```

## Pubsub ##

The publish / subscribe pattern is very popular in today's web development and design.  Chimera supports working with pubsub in a couple very useful ways.

- An Internal PubSub server implementation can be found in the chimera.pubsub.server unit.  In addition a WebBroker Producer component version is provided int he chimera.pubsub.webbroker unit.
- A Pubsub client implmentation that supports the internal pubsub server can be found in the chimera.pubsub.client unit.
- A Pubsub client implementation that is compatible with the Comet/J protocol as used in Faye's Ruby and Node Server in use in hundreds of thousands of implementations worldwide.

## RTL JSON Interop $$

Both the IJSONObject and the IJSONArray have methods to covert a Chimera JSON object to an RTL JSON Object:

```
  var jso := TJSON.New;
  var rtlo := jso.CreateRTLObject; // Results in a System.JSON.TJSONObject.  Note that this is a create function and lifetime must be managed on the resulting instance.
  
  var jsa := TJSONArray.New;
  var rtla := jsa.CreateRTLArray; // Results in a System.JSON.TJSONArray. This also is a create function and lifetime must be managed on the resulting instance.

```

Coverting from RTL JSON back to Chimera JSON is simple as well:

```
  var jsoOut := TJSON.From(rtlo);

  var jsoaOut := TJSONArray.From(rtla);
```

## License ##

Copyright 2022, by Jason Southwell

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

