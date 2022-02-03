
  var __create = Object.create;
  var __defProp = Object.defineProperty;
  var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __getProtoOf = Object.getPrototypeOf;
  var __hasOwnProp = Object.prototype.hasOwnProperty;
  var __markAsModule = (target) => __defProp(target, "__esModule", { value: true });
  var __commonJS = (cb, mod) => function __require() {
    return mod || (0, cb[Object.keys(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
  };
  var __reExport = (target, module, desc) => {
    if (module && typeof module === "object" || typeof module === "function") {
      for (let key of __getOwnPropNames(module))
        if (!__hasOwnProp.call(target, key) && key !== "default")
          __defProp(target, key, { get: () => module[key], enumerable: !(desc = __getOwnPropDesc(module, key)) || desc.enumerable });
    }
    return target;
  };
  var __toModule = (module) => {
    return __reExport(__markAsModule(__defProp(module != null ? __create(__getProtoOf(module)) : {}, "default", module && module.__esModule && "default" in module ? { get: () => module.default, enumerable: true } : { value: module, enumerable: true })), module);
  };

  // lodash.clonedeep/index.js
  var require_lodash = __commonJS({
    "lodash.clonedeep/index.js"(exports, module) {
      var LARGE_ARRAY_SIZE = 200;
      var HASH_UNDEFINED = "__lodash_hash_undefined__";
      var MAX_SAFE_INTEGER = 9007199254740991;
      var argsTag = "[object Arguments]";
      var arrayTag = "[object Array]";
      var boolTag = "[object Boolean]";
      var dateTag = "[object Date]";
      var errorTag = "[object Error]";
      var funcTag = "[object Function]";
      var genTag = "[object GeneratorFunction]";
      var mapTag = "[object Map]";
      var numberTag = "[object Number]";
      var objectTag = "[object Object]";
      var promiseTag = "[object Promise]";
      var regexpTag = "[object RegExp]";
      var setTag = "[object Set]";
      var stringTag = "[object String]";
      var symbolTag = "[object Symbol]";
      var weakMapTag = "[object WeakMap]";
      var arrayBufferTag = "[object ArrayBuffer]";
      var dataViewTag = "[object DataView]";
      var float32Tag = "[object Float32Array]";
      var float64Tag = "[object Float64Array]";
      var int8Tag = "[object Int8Array]";
      var int16Tag = "[object Int16Array]";
      var int32Tag = "[object Int32Array]";
      var uint8Tag = "[object Uint8Array]";
      var uint8ClampedTag = "[object Uint8ClampedArray]";
      var uint16Tag = "[object Uint16Array]";
      var uint32Tag = "[object Uint32Array]";
      var reRegExpChar = /[\\^$.*+?()[\]{}|]/g;
      var reFlags = /\w*$/;
      var reIsHostCtor = /^\[object .+?Constructor\]$/;
      var reIsUint = /^(?:0|[1-9]\d*)$/;
      var cloneableTags = {};
      cloneableTags[argsTag] = cloneableTags[arrayTag] = cloneableTags[arrayBufferTag] = cloneableTags[dataViewTag] = cloneableTags[boolTag] = cloneableTags[dateTag] = cloneableTags[float32Tag] = cloneableTags[float64Tag] = cloneableTags[int8Tag] = cloneableTags[int16Tag] = cloneableTags[int32Tag] = cloneableTags[mapTag] = cloneableTags[numberTag] = cloneableTags[objectTag] = cloneableTags[regexpTag] = cloneableTags[setTag] = cloneableTags[stringTag] = cloneableTags[symbolTag] = cloneableTags[uint8Tag] = cloneableTags[uint8ClampedTag] = cloneableTags[uint16Tag] = cloneableTags[uint32Tag] = true;
      cloneableTags[errorTag] = cloneableTags[funcTag] = cloneableTags[weakMapTag] = false;
      var freeGlobal = typeof global == "object" && global && global.Object === Object && global;
      var freeSelf = typeof self == "object" && self && self.Object === Object && self;
      var root = freeGlobal || freeSelf || Function("return this")();
      var freeExports = typeof exports == "object" && exports && !exports.nodeType && exports;
      var freeModule = freeExports && typeof module == "object" && module && !module.nodeType && module;
      var moduleExports = freeModule && freeModule.exports === freeExports;
      function addMapEntry(map, pair) {
        map.set(pair[0], pair[1]);
        return map;
      }
      function addSetEntry(set, value) {
        set.add(value);
        return set;
      }
      function arrayEach(array, iteratee) {
        var index = -1, length = array ? array.length : 0;
        while (++index < length) {
          if (iteratee(array[index], index, array) === false) {
            break;
          }
        }
        return array;
      }
      function arrayPush(array, values) {
        var index = -1, length = values.length, offset = array.length;
        while (++index < length) {
          array[offset + index] = values[index];
        }
        return array;
      }
      function arrayReduce(array, iteratee, accumulator, initAccum) {
        var index = -1, length = array ? array.length : 0;
        if (initAccum && length) {
          accumulator = array[++index];
        }
        while (++index < length) {
          accumulator = iteratee(accumulator, array[index], index, array);
        }
        return accumulator;
      }
      function baseTimes(n, iteratee) {
        var index = -1, result = Array(n);
        while (++index < n) {
          result[index] = iteratee(index);
        }
        return result;
      }
      function getValue(object, key) {
        return object == null ? void 0 : object[key];
      }
      function isHostObject(value) {
        var result = false;
        if (value != null && typeof value.toString != "function") {
          try {
            result = !!(value + "");
          } catch (e) {
          }
        }
        return result;
      }
      function mapToArray(map) {
        var index = -1, result = Array(map.size);
        map.forEach(function(value, key) {
          result[++index] = [key, value];
        });
        return result;
      }
      function overArg(func, transform) {
        return function(arg) {
          return func(transform(arg));
        };
      }
      function setToArray(set) {
        var index = -1, result = Array(set.size);
        set.forEach(function(value) {
          result[++index] = value;
        });
        return result;
      }
      var arrayProto = Array.prototype;
      var funcProto = Function.prototype;
      var objectProto = Object.prototype;
      var coreJsData = root["__core-js_shared__"];
      var maskSrcKey = function() {
        var uid = /[^.]+$/.exec(coreJsData && coreJsData.keys && coreJsData.keys.IE_PROTO || "");
        return uid ? "Symbol(src)_1." + uid : "";
      }();
      var funcToString = funcProto.toString;
      var hasOwnProperty = objectProto.hasOwnProperty;
      var objectToString = objectProto.toString;
      var reIsNative = RegExp("^" + funcToString.call(hasOwnProperty).replace(reRegExpChar, "\\$&").replace(/hasOwnProperty|(function).*?(?=\\\()| for .+?(?=\\\])/g, "$1.*?") + "$");
      var Buffer2 = moduleExports ? root.Buffer : void 0;
      var Symbol = root.Symbol;
      var Uint8Array2 = root.Uint8Array;
      var getPrototype = overArg(Object.getPrototypeOf, Object);
      var objectCreate = Object.create;
      var propertyIsEnumerable = objectProto.propertyIsEnumerable;
      var splice = arrayProto.splice;
      var nativeGetSymbols = Object.getOwnPropertySymbols;
      var nativeIsBuffer = Buffer2 ? Buffer2.isBuffer : void 0;
      var nativeKeys = overArg(Object.keys, Object);
      var DataView = getNative(root, "DataView");
      var Map = getNative(root, "Map");
      var Promise2 = getNative(root, "Promise");
      var Set = getNative(root, "Set");
      var WeakMap = getNative(root, "WeakMap");
      var nativeCreate = getNative(Object, "create");
      var dataViewCtorString = toSource(DataView);
      var mapCtorString = toSource(Map);
      var promiseCtorString = toSource(Promise2);
      var setCtorString = toSource(Set);
      var weakMapCtorString = toSource(WeakMap);
      var symbolProto = Symbol ? Symbol.prototype : void 0;
      var symbolValueOf = symbolProto ? symbolProto.valueOf : void 0;
      function Hash(entries) {
        var index = -1, length = entries ? entries.length : 0;
        this.clear();
        while (++index < length) {
          var entry = entries[index];
          this.set(entry[0], entry[1]);
        }
      }
      function hashClear() {
        this.__data__ = nativeCreate ? nativeCreate(null) : {};
      }
      function hashDelete(key) {
        return this.has(key) && delete this.__data__[key];
      }
      function hashGet(key) {
        var data = this.__data__;
        if (nativeCreate) {
          var result = data[key];
          return result === HASH_UNDEFINED ? void 0 : result;
        }
        return hasOwnProperty.call(data, key) ? data[key] : void 0;
      }
      function hashHas(key) {
        var data = this.__data__;
        return nativeCreate ? data[key] !== void 0 : hasOwnProperty.call(data, key);
      }
      function hashSet(key, value) {
        var data = this.__data__;
        data[key] = nativeCreate && value === void 0 ? HASH_UNDEFINED : value;
        return this;
      }
      Hash.prototype.clear = hashClear;
      Hash.prototype["delete"] = hashDelete;
      Hash.prototype.get = hashGet;
      Hash.prototype.has = hashHas;
      Hash.prototype.set = hashSet;
      function ListCache(entries) {
        var index = -1, length = entries ? entries.length : 0;
        this.clear();
        while (++index < length) {
          var entry = entries[index];
          this.set(entry[0], entry[1]);
        }
      }
      function listCacheClear() {
        this.__data__ = [];
      }
      function listCacheDelete(key) {
        var data = this.__data__, index = assocIndexOf(data, key);
        if (index < 0) {
          return false;
        }
        var lastIndex = data.length - 1;
        if (index == lastIndex) {
          data.pop();
        } else {
          splice.call(data, index, 1);
        }
        return true;
      }
      function listCacheGet(key) {
        var data = this.__data__, index = assocIndexOf(data, key);
        return index < 0 ? void 0 : data[index][1];
      }
      function listCacheHas(key) {
        return assocIndexOf(this.__data__, key) > -1;
      }
      function listCacheSet(key, value) {
        var data = this.__data__, index = assocIndexOf(data, key);
        if (index < 0) {
          data.push([key, value]);
        } else {
          data[index][1] = value;
        }
        return this;
      }
      ListCache.prototype.clear = listCacheClear;
      ListCache.prototype["delete"] = listCacheDelete;
      ListCache.prototype.get = listCacheGet;
      ListCache.prototype.has = listCacheHas;
      ListCache.prototype.set = listCacheSet;
      function MapCache(entries) {
        var index = -1, length = entries ? entries.length : 0;
        this.clear();
        while (++index < length) {
          var entry = entries[index];
          this.set(entry[0], entry[1]);
        }
      }
      function mapCacheClear() {
        this.__data__ = {
          "hash": new Hash(),
          "map": new (Map || ListCache)(),
          "string": new Hash()
        };
      }
      function mapCacheDelete(key) {
        return getMapData(this, key)["delete"](key);
      }
      function mapCacheGet(key) {
        return getMapData(this, key).get(key);
      }
      function mapCacheHas(key) {
        return getMapData(this, key).has(key);
      }
      function mapCacheSet(key, value) {
        getMapData(this, key).set(key, value);
        return this;
      }
      MapCache.prototype.clear = mapCacheClear;
      MapCache.prototype["delete"] = mapCacheDelete;
      MapCache.prototype.get = mapCacheGet;
      MapCache.prototype.has = mapCacheHas;
      MapCache.prototype.set = mapCacheSet;
      function Stack(entries) {
        this.__data__ = new ListCache(entries);
      }
      function stackClear() {
        this.__data__ = new ListCache();
      }
      function stackDelete(key) {
        return this.__data__["delete"](key);
      }
      function stackGet(key) {
        return this.__data__.get(key);
      }
      function stackHas(key) {
        return this.__data__.has(key);
      }
      function stackSet(key, value) {
        var cache = this.__data__;
        if (cache instanceof ListCache) {
          var pairs = cache.__data__;
          if (!Map || pairs.length < LARGE_ARRAY_SIZE - 1) {
            pairs.push([key, value]);
            return this;
          }
          cache = this.__data__ = new MapCache(pairs);
        }
        cache.set(key, value);
        return this;
      }
      Stack.prototype.clear = stackClear;
      Stack.prototype["delete"] = stackDelete;
      Stack.prototype.get = stackGet;
      Stack.prototype.has = stackHas;
      Stack.prototype.set = stackSet;
      function arrayLikeKeys(value, inherited) {
        var result = isArray(value) || isArguments(value) ? baseTimes(value.length, String) : [];
        var length = result.length, skipIndexes = !!length;
        for (var key in value) {
          if ((inherited || hasOwnProperty.call(value, key)) && !(skipIndexes && (key == "length" || isIndex(key, length)))) {
            result.push(key);
          }
        }
        return result;
      }
      function assignValue(object, key, value) {
        var objValue = object[key];
        if (!(hasOwnProperty.call(object, key) && eq(objValue, value)) || value === void 0 && !(key in object)) {
          object[key] = value;
        }
      }
      function assocIndexOf(array, key) {
        var length = array.length;
        while (length--) {
          if (eq(array[length][0], key)) {
            return length;
          }
        }
        return -1;
      }
      function baseAssign(object, source) {
        return object && copyObject(source, keys(source), object);
      }
      function baseClone(value, isDeep, isFull, customizer, key, object, stack) {
        var result;
        if (customizer) {
          result = object ? customizer(value, key, object, stack) : customizer(value);
        }
        if (result !== void 0) {
          return result;
        }
        if (!isObject(value)) {
          return value;
        }
        var isArr = isArray(value);
        if (isArr) {
          result = initCloneArray(value);
          if (!isDeep) {
            return copyArray(value, result);
          }
        } else {
          var tag = getTag(value), isFunc = tag == funcTag || tag == genTag;
          if (isBuffer(value)) {
            return cloneBuffer(value, isDeep);
          }
          if (tag == objectTag || tag == argsTag || isFunc && !object) {
            if (isHostObject(value)) {
              return object ? value : {};
            }
            result = initCloneObject(isFunc ? {} : value);
            if (!isDeep) {
              return copySymbols(value, baseAssign(result, value));
            }
          } else {
            if (!cloneableTags[tag]) {
              return object ? value : {};
            }
            result = initCloneByTag(value, tag, baseClone, isDeep);
          }
        }
        stack || (stack = new Stack());
        var stacked = stack.get(value);
        if (stacked) {
          return stacked;
        }
        stack.set(value, result);
        if (!isArr) {
          var props = isFull ? getAllKeys(value) : keys(value);
        }
        arrayEach(props || value, function(subValue, key2) {
          if (props) {
            key2 = subValue;
            subValue = value[key2];
          }
          assignValue(result, key2, baseClone(subValue, isDeep, isFull, customizer, key2, value, stack));
        });
        return result;
      }
      function baseCreate(proto) {
        return isObject(proto) ? objectCreate(proto) : {};
      }
      function baseGetAllKeys(object, keysFunc, symbolsFunc) {
        var result = keysFunc(object);
        return isArray(object) ? result : arrayPush(result, symbolsFunc(object));
      }
      function baseGetTag(value) {
        return objectToString.call(value);
      }
      function baseIsNative(value) {
        if (!isObject(value) || isMasked(value)) {
          return false;
        }
        var pattern = isFunction(value) || isHostObject(value) ? reIsNative : reIsHostCtor;
        return pattern.test(toSource(value));
      }
      function baseKeys(object) {
        if (!isPrototype(object)) {
          return nativeKeys(object);
        }
        var result = [];
        for (var key in Object(object)) {
          if (hasOwnProperty.call(object, key) && key != "constructor") {
            result.push(key);
          }
        }
        return result;
      }
      function cloneBuffer(buffer, isDeep) {
        if (isDeep) {
          return buffer.slice();
        }
        var result = new buffer.constructor(buffer.length);
        buffer.copy(result);
        return result;
      }
      function cloneArrayBuffer(arrayBuffer) {
        var result = new arrayBuffer.constructor(arrayBuffer.byteLength);
        new Uint8Array2(result).set(new Uint8Array2(arrayBuffer));
        return result;
      }
      function cloneDataView(dataView, isDeep) {
        var buffer = isDeep ? cloneArrayBuffer(dataView.buffer) : dataView.buffer;
        return new dataView.constructor(buffer, dataView.byteOffset, dataView.byteLength);
      }
      function cloneMap(map, isDeep, cloneFunc) {
        var array = isDeep ? cloneFunc(mapToArray(map), true) : mapToArray(map);
        return arrayReduce(array, addMapEntry, new map.constructor());
      }
      function cloneRegExp(regexp) {
        var result = new regexp.constructor(regexp.source, reFlags.exec(regexp));
        result.lastIndex = regexp.lastIndex;
        return result;
      }
      function cloneSet(set, isDeep, cloneFunc) {
        var array = isDeep ? cloneFunc(setToArray(set), true) : setToArray(set);
        return arrayReduce(array, addSetEntry, new set.constructor());
      }
      function cloneSymbol(symbol) {
        return symbolValueOf ? Object(symbolValueOf.call(symbol)) : {};
      }
      function cloneTypedArray(typedArray, isDeep) {
        var buffer = isDeep ? cloneArrayBuffer(typedArray.buffer) : typedArray.buffer;
        return new typedArray.constructor(buffer, typedArray.byteOffset, typedArray.length);
      }
      function copyArray(source, array) {
        var index = -1, length = source.length;
        array || (array = Array(length));
        while (++index < length) {
          array[index] = source[index];
        }
        return array;
      }
      function copyObject(source, props, object, customizer) {
        object || (object = {});
        var index = -1, length = props.length;
        while (++index < length) {
          var key = props[index];
          var newValue = customizer ? customizer(object[key], source[key], key, object, source) : void 0;
          assignValue(object, key, newValue === void 0 ? source[key] : newValue);
        }
        return object;
      }
      function copySymbols(source, object) {
        return copyObject(source, getSymbols(source), object);
      }
      function getAllKeys(object) {
        return baseGetAllKeys(object, keys, getSymbols);
      }
      function getMapData(map, key) {
        var data = map.__data__;
        return isKeyable(key) ? data[typeof key == "string" ? "string" : "hash"] : data.map;
      }
      function getNative(object, key) {
        var value = getValue(object, key);
        return baseIsNative(value) ? value : void 0;
      }
      var getSymbols = nativeGetSymbols ? overArg(nativeGetSymbols, Object) : stubArray;
      var getTag = baseGetTag;
      if (DataView && getTag(new DataView(new ArrayBuffer(1))) != dataViewTag || Map && getTag(new Map()) != mapTag || Promise2 && getTag(Promise2.resolve()) != promiseTag || Set && getTag(new Set()) != setTag || WeakMap && getTag(new WeakMap()) != weakMapTag) {
        getTag = function(value) {
          var result = objectToString.call(value), Ctor = result == objectTag ? value.constructor : void 0, ctorString = Ctor ? toSource(Ctor) : void 0;
          if (ctorString) {
            switch (ctorString) {
              case dataViewCtorString:
                return dataViewTag;
              case mapCtorString:
                return mapTag;
              case promiseCtorString:
                return promiseTag;
              case setCtorString:
                return setTag;
              case weakMapCtorString:
                return weakMapTag;
            }
          }
          return result;
        };
      }
      function initCloneArray(array) {
        var length = array.length, result = array.constructor(length);
        if (length && typeof array[0] == "string" && hasOwnProperty.call(array, "index")) {
          result.index = array.index;
          result.input = array.input;
        }
        return result;
      }
      function initCloneObject(object) {
        return typeof object.constructor == "function" && !isPrototype(object) ? baseCreate(getPrototype(object)) : {};
      }
      function initCloneByTag(object, tag, cloneFunc, isDeep) {
        var Ctor = object.constructor;
        switch (tag) {
          case arrayBufferTag:
            return cloneArrayBuffer(object);
          case boolTag:
          case dateTag:
            return new Ctor(+object);
          case dataViewTag:
            return cloneDataView(object, isDeep);
          case float32Tag:
          case float64Tag:
          case int8Tag:
          case int16Tag:
          case int32Tag:
          case uint8Tag:
          case uint8ClampedTag:
          case uint16Tag:
          case uint32Tag:
            return cloneTypedArray(object, isDeep);
          case mapTag:
            return cloneMap(object, isDeep, cloneFunc);
          case numberTag:
          case stringTag:
            return new Ctor(object);
          case regexpTag:
            return cloneRegExp(object);
          case setTag:
            return cloneSet(object, isDeep, cloneFunc);
          case symbolTag:
            return cloneSymbol(object);
        }
      }
      function isIndex(value, length) {
        length = length == null ? MAX_SAFE_INTEGER : length;
        return !!length && (typeof value == "number" || reIsUint.test(value)) && (value > -1 && value % 1 == 0 && value < length);
      }
      function isKeyable(value) {
        var type = typeof value;
        return type == "string" || type == "number" || type == "symbol" || type == "boolean" ? value !== "__proto__" : value === null;
      }
      function isMasked(func) {
        return !!maskSrcKey && maskSrcKey in func;
      }
      function isPrototype(value) {
        var Ctor = value && value.constructor, proto = typeof Ctor == "function" && Ctor.prototype || objectProto;
        return value === proto;
      }
      function toSource(func) {
        if (func != null) {
          try {
            return funcToString.call(func);
          } catch (e) {
          }
          try {
            return func + "";
          } catch (e) {
          }
        }
        return "";
      }
      function cloneDeep(value) {
        return baseClone(value, true, true);
      }
      function eq(value, other) {
        return value === other || value !== value && other !== other;
      }
      function isArguments(value) {
        return isArrayLikeObject(value) && hasOwnProperty.call(value, "callee") && (!propertyIsEnumerable.call(value, "callee") || objectToString.call(value) == argsTag);
      }
      var isArray = Array.isArray;
      function isArrayLike(value) {
        return value != null && isLength(value.length) && !isFunction(value);
      }
      function isArrayLikeObject(value) {
        return isObjectLike(value) && isArrayLike(value);
      }
      var isBuffer = nativeIsBuffer || stubFalse;
      function isFunction(value) {
        var tag = isObject(value) ? objectToString.call(value) : "";
        return tag == funcTag || tag == genTag;
      }
      function isLength(value) {
        return typeof value == "number" && value > -1 && value % 1 == 0 && value <= MAX_SAFE_INTEGER;
      }
      function isObject(value) {
        var type = typeof value;
        return !!value && (type == "object" || type == "function");
      }
      function isObjectLike(value) {
        return !!value && typeof value == "object";
      }
      function keys(object) {
        return isArrayLike(object) ? arrayLikeKeys(object) : baseKeys(object);
      }
      function stubArray() {
        return [];
      }
      function stubFalse() {
        return false;
      }
      module.exports = cloneDeep;
    }
  });

  // codemirror-record/src/utils/origin.js
  var origin = {
    "*compose": "c",
    "+delete": "d",
    "+input": "i",
    "markText": "k",
    "select": "l",
    "*mouse": "m",
    "*rename": "n",
    "+move": "o",
    "paste": "p",
    "drag": "r",
    "setValue": "s",
    "cut": "x",
    "extra": "e"
  };
  var origin_default = {
    encode: function(fullname) {
      return origin[fullname];
    },
    decode: function(abbreviation) {
      for (const key in origin) {
        if (origin[key] === abbreviation) {
          return key;
        }
      }
      return "unknown";
    }
  };

  // codemirror-record/src/utils/minify.js
  function getInterval(operation) {
    if (operation.from.line === operation.to.line && operation.from.ch === operation.to.ch) {
      return [operation.from.line, operation.from.ch];
    } else {
      return [
        [operation.from.line, operation.from.ch],
        [operation.to.line, operation.to.ch]
      ];
    }
  }
  function minifyTime(timeInterval) {
    if (timeInterval[0] === timeInterval[1]) {
      return timeInterval[0];
    } else {
      return timeInterval;
    }
  }
  function minify_default(operations) {
    const minifiedOperations = [];
    while (operations.length > 0) {
      const operation = operations.pop();
      for (let i = 0; i < operation.ops.length; i++) {
        operation.ops[i].o = origin_default.encode(operation.ops[i].origin);
        if (operation.ops[i].origin !== "extra") {
          operation.ops[i].i = getInterval(operation.ops[i]);
          operation.ops[i].a = operation.ops[i].text;
          operation.ops[i].d = operation.ops[i].removed;
          if (operation.ops[i].a.length === 1 && operation.ops[i].a[0] === "") {
            delete operation.ops[i].a;
          }
          if (operation.ops[i].d.length === 1 && operation.ops[i].d[0] === "") {
            delete operation.ops[i].d;
          }
          if ("select" in operation.ops[i]) {
            operation.ops[i].s = operation.ops[i].select;
            delete operation.ops[i].select;
          }
        }
        if (operation.combo === 1) {
          delete operation.ops[i].d;
        }
        delete operation.ops[i].removed;
        delete operation.ops[i].text;
        delete operation.ops[i].from;
        delete operation.ops[i].origin;
        delete operation.ops[i].to;
      }
      operation.t = minifyTime([operation.startTime, operation.endTime]);
      operation.l = operation.combo;
      operation.o = operation.ops;
      if (operation.l === 1) {
        delete operation.l;
      }
      delete operation.ops;
      delete operation.delayDuration;
      delete operation.combo;
      delete operation.startTime;
      delete operation.endTime;
      minifiedOperations.unshift(operation);
    }
    return minifiedOperations;
  }

  // codemirror-record/src/func/compress/compose.js
  function isContinueCompose(firstChange, secondChange) {
    if (firstChange.ops.length !== secondChange.ops.length) {
      return false;
    } else {
      for (let i = 0; i < secondChange.ops.length; i++) {
        const firstOp = firstChange.ops[i];
        const secondOp = secondChange.ops[i];
        if (secondOp.from.line !== secondOp.to.line || firstOp.from.line !== firstOp.to.line || secondOp.from.ch !== secondOp.to.ch || firstOp.from.ch !== firstOp.to.ch) {
          return false;
        } else if (firstOp.from.ch + firstOp.text[0].length !== secondOp.from.ch) {
          return false;
        }
      }
    }
    return true;
  }
  function compressContinuousCompose(changes) {
    const newChanges = [];
    while (changes.length > 0) {
      const change = changes.pop();
      if (change.ops[0].origin === "*compose") {
        while (changes.length > 0) {
          const lastChange = changes.pop();
          if (lastChange.ops[0].origin === "*compose" && isContinueCompose(lastChange, change)) {
            change.startTime = lastChange.startTime;
            change.delayDuration = lastChange.delayDuration;
            change.combo += 1;
            for (let i = 0; i < change.ops.length; i++) {
              change.ops[i].from = lastChange.ops[i].from;
              change.ops[i].to = lastChange.ops[i].to;
              change.ops[i].text = lastChange.ops[i].text.concat(change.ops[i].text);
            }
          } else {
            changes.push(lastChange);
            break;
          }
        }
        newChanges.unshift(change);
      } else {
        newChanges.unshift(change);
      }
    }
    return newChanges;
  }
  function compose_default(changes) {
    return compressContinuousCompose(changes);
  }

  // codemirror-record/src/config.js
  var config_default = {
    acceptableMinOperationDelay: 1200,
    acceptableMinCursorMoveDelay: 800,
    maxDelayBetweenOperations: 0
  };

  // codemirror-record/src/func/compress/cursor.js
  var longDelayCount = 0;
  var longDelayAverage = 0;
  function combineLongDelayCursorMove(firstChange, secondChange) {
    const minCursorMoveDelay = config_default.acceptableMinCursorMoveDelay;
    if (firstChange.delayDuration >= minCursorMoveDelay && isLongDelayCursorMove(secondChange)) {
      longDelayAverage = (longDelayAverage * longDelayCount + secondChange.delayDuration) / (longDelayCount + 1);
      longDelayCount++;
      return true;
    }
    longDelayCount = 0;
    longDelayAverage = 0;
    return false;
  }
  function isLongDelayCursorMove(secondChange) {
    const halfCursorMoveDelay = config_default.acceptableMinCursorMoveDelay / 2;
    const delayDuration = secondChange.delayDuration;
    if (longDelayCount !== 0) {
      if (delayDuration >= longDelayAverage + halfCursorMoveDelay) {
        return false;
      } else if (delayDuration <= longDelayAverage - halfCursorMoveDelay) {
        return false;
      }
    }
    return true;
  }
  function areCursorsPositionsContinue(firstChange, secondChange, direction) {
    for (let i = 0; i < secondChange.crs.length; i++) {
      const firstCh = firstChange.crs[i];
      const secondCh = secondChange.crs[i];
      if (firstCh.anchor.line !== firstCh.head.line || firstCh.anchor.ch !== firstCh.head.ch) {
        return false;
      } else if (firstChange.crs[i].anchor.ch + direction !== secondCh.anchor.ch) {
        return false;
      } else if (firstChange.crs[i].anchor.line !== secondCh.anchor.line) {
        return false;
      }
    }
    return true;
  }
  function isContinueCursorMove(firstChange, secondChange, direction = 1) {
    const minCursorMoveDelay = config_default.acceptableMinCursorMoveDelay;
    if (firstChange.crs.length !== secondChange.crs.length) {
      return false;
    } else if (secondChange.delayDuration >= minCursorMoveDelay) {
      if (!combineLongDelayCursorMove(firstChange, secondChange)) {
        return false;
      }
    } else if (firstChange.delayDuration >= minCursorMoveDelay) {
      return false;
    }
    if (!areCursorsPositionsContinue(firstChange, secondChange, direction)) {
      return false;
    }
    return true;
  }
  function compressContinuousCursorMove(operations, direction = 1) {
    const newOperations = [];
    while (operations.length > 0) {
      const operation = operations.pop();
      if ("crs" in operation) {
        while (operations.length > 0) {
          const lastOperation = operations.pop();
          if ("crs" in lastOperation && isContinueCursorMove(lastOperation, operation, direction)) {
            operation.startTime = lastOperation.startTime;
            operation.delayDuration = lastOperation.delayDuration;
            operation.combo += 1;
            for (let i = 0; i < operation.crs.length; i++) {
              operation.crs[i].anchor = lastOperation.crs[i].anchor;
            }
          } else {
            operations.push(lastOperation);
            break;
          }
        }
        newOperations.unshift(operation);
      } else {
        newOperations.unshift(operation);
      }
    }
    return newOperations;
  }
  function convertCursorMoveFormat(operations) {
    for (let i = 0; i < operations.length; i++) {
      if ("crs" in operations[i]) {
        operations[i].ops = [];
        for (let j = 0; j < operations[i].crs.length; j++) {
          operations[i].ops.push({
            from: operations[i].crs[j].anchor,
            to: operations[i].crs[j].head,
            origin: "+move",
            text: [""],
            removed: [""]
          });
        }
        delete operations[i].crs;
      }
    }
    return operations;
  }
  function cursor_default(operations) {
    operations = compressContinuousCursorMove(operations, 1);
    operations = compressContinuousCursorMove(operations, -1);
    operations = convertCursorMoveFormat(operations);
    return operations;
  }

  // codemirror-record/src/func/compress/input.js
  var longDelayCount2 = 0;
  var longDelayAverage2 = 0;
  function combineLongDelayInput(firstChange, secondChange) {
    const minOperationDelay = config_default.acceptableMinOperationDelay;
    if (firstChange.delayDuration >= minOperationDelay && isLongDelayInput(secondChange)) {
      longDelayAverage2 = (longDelayAverage2 * longDelayCount2 + secondChange.delayDuration) / (longDelayCount2 + 1);
      longDelayCount2++;
      return true;
    }
    longDelayCount2 = 0;
    longDelayAverage2 = 0;
    return false;
  }
  function isLongDelayInput(secondChange) {
    const halfOperationDelay = config_default.acceptableMinOperationDelay / 2;
    const delayDuration = secondChange.delayDuration;
    if (longDelayCount2 !== 0) {
      if (delayDuration >= longDelayAverage2 + halfOperationDelay) {
        return false;
      } else if (delayDuration <= longDelayAverage2 - halfOperationDelay) {
        return false;
      }
    }
    return true;
  }
  function areInputPositionsContinue(firstChange, secondChange) {
    for (let i = 0; i < secondChange.ops.length; i++) {
      const firstOp = firstChange.ops[i];
      const secondOp = secondChange.ops[i];
      if (firstOp.text.length !== 1) {
        return false;
      }
      if (secondOp.from.line !== secondOp.to.line || firstOp.from.line !== firstOp.to.line || secondOp.from.ch !== secondOp.to.ch || firstOp.from.ch !== firstOp.to.ch) {
        return false;
      } else if (firstOp.from.ch + 1 !== secondOp.from.ch && !(firstOp.from.line + 1 === secondOp.from.line && secondOp.from.ch === 0)) {
        return false;
      }
    }
    return true;
  }
  function isContinueInput(firstChange, secondChange) {
    const minOperationDelay = config_default.acceptableMinOperationDelay;
    if (firstChange.ops.length !== secondChange.ops.length) {
      return false;
    } else if (secondChange.delayDuration >= minOperationDelay) {
      if (!combineLongDelayInput(firstChange, secondChange)) {
        return false;
      }
    } else if (firstChange.delayDuration >= minOperationDelay) {
      return false;
    }
    if (!areInputPositionsContinue(firstChange, secondChange)) {
      return false;
    }
    return true;
  }
  function compressOperationsTexts(change) {
    for (let i = 0; i < change.ops.length; i++) {
      let compressedTexts = "";
      for (let j = 0; j < change.ops[i].text.length; j++) {
        if (change.ops[i].text[j] !== "") {
          compressedTexts += change.ops[i].text[j];
        } else if (j + 1 < change.ops[i].text.length && change.ops[i].text[j + 1] === "") {
          compressedTexts += "\n";
        }
      }
      change.ops[i].text = compressedTexts;
    }
    return change;
  }
  function hasAutoClosePair(change) {
    const closePairs = ["()", "[]", "{}", "''", '""'];
    for (let i = 0; i < change.ops.length; i++) {
      for (let j = 0; j < change.ops[i].text.length; j++) {
        if (closePairs.indexOf(change.ops[i].text[j]) >= 0) {
          return true;
        }
      }
    }
    return false;
  }
  function compressContinuousInput(changes) {
    const newChanges = [];
    while (changes.length > 0) {
      let change = changes.pop();
      if (change.ops[0].origin === "+input" && !hasAutoClosePair(change)) {
        while (changes.length > 0) {
          const lastChange = changes.pop();
          if (lastChange.ops[0].origin === "+input" && !hasAutoClosePair(lastChange) && isContinueInput(lastChange, change)) {
            change.startTime = lastChange.startTime;
            change.delayDuration = lastChange.delayDuration;
            change.combo += 1;
            for (let i = 0; i < change.ops.length; i++) {
              change.ops[i].from = lastChange.ops[i].from;
              change.ops[i].to = lastChange.ops[i].to;
              change.ops[i].text = lastChange.ops[i].text.concat(change.ops[i].text);
            }
          } else {
            changes.push(lastChange);
            break;
          }
        }
        change = compressOperationsTexts(change);
        newChanges.unshift(change);
      } else {
        newChanges.unshift(change);
      }
    }
    return newChanges;
  }
  function input_default(changes) {
    return compressContinuousInput(changes);
  }

  // codemirror-record/src/func/compress/select.js
  var import_lodash = __toModule(require_lodash());
  var longDelayCount3 = 0;
  var longDelayAverage3 = 0;
  function combineLongDelaySelect(firstChange, secondChange) {
    const minCursorMoveDelay = config_default.acceptableMinCursorMoveDelay;
    if (firstChange.delayDuration >= minCursorMoveDelay && isLongDelaySelect(secondChange)) {
      longDelayAverage3 = (longDelayAverage3 * longDelayCount3 + secondChange.delayDuration) / (longDelayCount3 + 1);
      longDelayCount3++;
      return true;
    }
    longDelayCount3 = 0;
    longDelayAverage3 = 0;
    return false;
  }
  function isLongDelaySelect(secondChange) {
    const halfCursorMoveDelay = config_default.acceptableMinCursorMoveDelay / 2;
    const delayDuration = secondChange.delayDuration;
    if (longDelayCount3 !== 0) {
      if (delayDuration >= longDelayAverage3 + halfCursorMoveDelay) {
        return false;
      } else if (delayDuration <= longDelayAverage3 - halfCursorMoveDelay) {
        return false;
      }
    }
    return true;
  }
  function areSelectionsHeadsContinue(firstChange, secondChange) {
    for (let i = 0; i < secondChange.crs.length; i++) {
      const firstCh = firstChange.crs[i];
      const secondCh = secondChange.crs[i];
      if (secondCh.anchor.line === secondCh.head.line && secondCh.anchor.ch === secondCh.head.ch) {
        return false;
      } else if (firstCh.anchor.line !== secondCh.anchor.line || firstCh.anchor.ch !== secondCh.anchor.ch) {
        return false;
      }
    }
    return true;
  }
  function isContinueSelect(firstChange, secondChange) {
    const minCursorMoveDelay = config_default.acceptableMinCursorMoveDelay;
    if (firstChange.crs.length !== secondChange.crs.length) {
      return false;
    } else if (secondChange.delayDuration >= minCursorMoveDelay) {
      if (!combineLongDelaySelect(firstChange, secondChange)) {
        return false;
      }
    } else if (firstChange.delayDuration >= minCursorMoveDelay) {
      return false;
    }
    if (!areSelectionsHeadsContinue(firstChange, secondChange)) {
      return false;
    }
    return true;
  }
  function compressSelectionHeads(heads) {
    const resultArray = [];
    let currentLine = -1;
    while (heads.length > 0) {
      const head = heads.shift();
      if (currentLine !== head.line) {
        resultArray.push([head.line]);
        currentLine = head.line;
      }
      resultArray[resultArray.length - 1].push(head.ch);
    }
    for (let i = 0; i < resultArray.length; i++) {
      let chsInterval = resultArray[i].slice(1);
      chsInterval = convertChsToInterval(chsInterval);
      chsInterval = convertChsToInterval(chsInterval, -1);
      resultArray[i] = [resultArray[i][0], chsInterval];
    }
    return resultArray;
  }
  function convertChsToInterval(chs, direction = 1) {
    const resultArray = [];
    while (chs.length > 0) {
      const current = chs.shift();
      if (typeof current !== "number") {
        resultArray.push(current);
      } else if (resultArray.length === 0 || Array.isArray(resultArray[resultArray.length - 1])) {
        resultArray.push({ from: current, to: current });
      } else if ("to" in resultArray[resultArray.length - 1]) {
        if (resultArray[resultArray.length - 1].to + direction !== current) {
          resultArray.push({ from: current, to: current });
        } else {
          resultArray[resultArray.length - 1].to = current;
        }
      }
    }
    for (let i = 0; i < resultArray.length; i++) {
      if ("to" in resultArray[i]) {
        if (resultArray[i].from === resultArray[i].to) {
          resultArray[i] = resultArray[i].from;
        } else {
          resultArray[i] = [resultArray[i].from, resultArray[i].to];
        }
      }
    }
    return resultArray;
  }
  function compressContinuousSelect(operations, direction = 1) {
    const newOperations = [];
    while (operations.length > 0) {
      const operation = (0, import_lodash.default)(operations.pop());
      if ("crs" in operation) {
        while (operations.length > 0) {
          const lastOperation = (0, import_lodash.default)(operations.pop());
          if ("crs" in lastOperation && isContinueSelect(lastOperation, operation)) {
            operation.startTime = lastOperation.startTime;
            operation.delayDuration = lastOperation.delayDuration;
            operation.combo += 1;
            for (let i = 0; i < operation.crs.length; i++) {
              if (!("heads" in operation.crs[i])) {
                operation.crs[i].heads = [lastOperation.crs[i].head, operation.crs[i].head];
              } else {
                operation.crs[i].heads.unshift(lastOperation.crs[i].head);
              }
            }
          } else {
            operations.push(lastOperation);
            break;
          }
        }
        newOperations.unshift(operation);
      } else {
        newOperations.unshift(operation);
      }
    }
    return newOperations;
  }
  function convertSelectFormat(operations) {
    for (let i = 0; i < operations.length; i++) {
      if ("crs" in operations[i] && operations[i].combo > 1) {
        operations[i].ops = [];
        for (let j = 0; j < operations[i].crs.length; j++) {
          operations[i].ops.push({
            from: operations[i].crs[j].anchor,
            to: operations[i].crs[j].anchor,
            origin: "select",
            text: [""],
            removed: [""],
            select: compressSelectionHeads(operations[i].crs[j].heads)
          });
        }
        delete operations[i].crs;
      }
    }
    return operations;
  }
  function select_default(operations) {
    operations = compressContinuousSelect(operations);
    operations = convertSelectFormat(operations);
    return operations;
  }

  // codemirror-record/src/func/compress/remove.js
  var longDelayCount4 = 0;
  var longDelayAverage4 = 0;
  function combineLongDelayDelete(firstChange, secondChange) {
    const minOperationDelay = config_default.acceptableMinOperationDelay;
    if (firstChange.delayDuration >= minOperationDelay && isLongDelayDelete(secondChange)) {
      longDelayAverage4 = (longDelayAverage4 * longDelayCount4 + secondChange.delayDuration) / (longDelayCount4 + 1);
      longDelayCount4++;
      return true;
    }
    longDelayCount4 = 0;
    longDelayAverage4 = 0;
    return false;
  }
  function isLongDelayDelete(secondChange) {
    const halfOperationDelay = config_default.acceptableMinOperationDelay / 2;
    const delayDuration = secondChange.delayDuration;
    if (longDelayCount4 !== 0) {
      if (delayDuration >= longDelayAverage4 + halfOperationDelay) {
        return false;
      } else if (delayDuration <= longDelayAverage4 - halfOperationDelay) {
        return false;
      }
    }
    return true;
  }
  function areDeletePositionsContinue(firstChange, secondChange) {
    for (let i = 0; i < secondChange.ops.length; i++) {
      const firstOp = firstChange.ops[i];
      const secondOp = secondChange.ops[i];
      if (firstOp.from.ch !== secondOp.to.ch || firstOp.from.line !== secondOp.to.line) {
        return false;
      }
    }
    return true;
  }
  function isContinueDelete(firstChange, secondChange) {
    const minOperationDelay = config_default.acceptableMinOperationDelay;
    if (firstChange.ops.length !== secondChange.ops.length) {
      return false;
    } else if (secondChange.delayDuration >= minOperationDelay) {
      if (!combineLongDelayDelete(firstChange, secondChange)) {
        return false;
      }
    } else if (firstChange.delayDuration >= minOperationDelay) {
      return false;
    }
    if (!areDeletePositionsContinue(firstChange, secondChange)) {
      return false;
    }
    return true;
  }
  function compressOperationsRemovals(change) {
    if (change.combo === 1)
      return change;
    for (let i = 0; i < change.ops.length; i++) {
      const resultArray = [];
      let countStack = [];
      while (change.ops[i].removed.length > 0) {
        const head = change.ops[i].removed.shift();
        if (typeof head === "string") {
          if (countStack.length === 0) {
            countStack.push(head);
          } else {
            if (countStack[0].length === head.length) {
              countStack.push(head);
            } else {
              resultArray.push([countStack[0].length, countStack.length]);
              countStack = [];
              countStack.push(head);
            }
          }
        } else {
          if (countStack.length > 0) {
            resultArray.push([countStack[0].length, countStack.length]);
            countStack = [];
          }
          resultArray.push([
            [head[0].line, head[0].ch],
            [head[1].line, head[1].ch]
          ]);
        }
      }
      if (countStack.length > 0) {
        resultArray.push([countStack[0].length, countStack.length]);
      }
      change.ops[i].removed = resultArray;
    }
    return change;
  }
  function compressContinuousDelete(changes) {
    const newChanges = [];
    while (changes.length > 0) {
      let change = changes.pop();
      if (change.ops[0].origin === "+delete") {
        while (changes.length > 0) {
          const lastChange = changes.pop();
          if (lastChange.ops[0].origin === "+delete" && isContinueDelete(lastChange, change)) {
            change.startTime = lastChange.startTime;
            change.delayDuration = lastChange.delayDuration;
            for (let i = 0; i < change.ops.length; i++) {
              if (change.combo === 1 && change.ops[i].removed.length > 1) {
                change.ops[i].removed = [[change.ops[i].from, change.ops[i].to]];
              }
              if (lastChange.ops[i].removed.length > 1) {
                lastChange.ops[i].removed = [
                  [lastChange.ops[i].from, lastChange.ops[i].to]
                ];
              }
              change.ops[i].removed = change.ops[i].removed.concat(lastChange.ops[i].removed);
              change.ops[i].to = lastChange.ops[i].to;
            }
            change.combo += 1;
          } else {
            changes.push(lastChange);
            break;
          }
        }
        change = compressOperationsRemovals(change);
        newChanges.unshift(change);
      } else {
        newChanges.unshift(change);
      }
    }
    return newChanges;
  }
  function remove_default(changes) {
    return compressContinuousDelete(changes);
  }

  // codemirror-record/src/func/compress/index.js
  var compress_default = {
    compose: compose_default,
    cursor: cursor_default,
    input: input_default,
    select: select_default,
    remove: remove_default
  };

  // codemirror-record/src/CodeRecord.js
  var CodeRecord = class {
    constructor(editor) {
      this.initTime = +new Date();
      this.lastChangeTime = +new Date();
      this.lastCursorActivityTime = +new Date();
      this.operations = [];
      this.editor = editor;
      this.changesListener = this.changesListener.bind(this);
      this.cursorActivityListener = this.cursorActivityListener.bind(this);
      this.swapDocListener = this.swapDocListener.bind(this);
    }
    recordExtraActivity(activity) {
      const changes = [{
        origin: "extra",
        activity
      }];
      this.operations.push({
        startTime: this.getOperationRelativeTime(),
        endTime: this.getOperationRelativeTime(),
        ops: changes
      });
    }
    listen() {
      this.editor.on("changes", this.changesListener);
      this.editor.on("swapDoc", this.swapDocListener);
      this.editor.on("cursorActivity", this.cursorActivityListener);
    }
    getRecords() {
      this.removeRedundantCursorOperations();
      this.compressCursorOperations();
      this.compressChanges();
      return JSON.stringify(minify_default(this.operations));
    }
    getOperationRelativeTime() {
      const currentTime = +new Date();
      return currentTime - this.initTime;
    }
    getLastChangePause() {
      const currentTime = +new Date();
      const lastChangePause = currentTime - this.lastChangeTime;
      this.lastChangeTime = currentTime;
      return lastChangePause;
    }
    getLastCursorActivityPause() {
      const currentTime = +new Date();
      const lastCursorActivityPause = currentTime - this.lastCursorActivityTime;
      this.lastCursorActivityTime = currentTime;
      return lastCursorActivityPause;
    }
    changesListener(editor, changes) {
      this.operations.push({
        startTime: this.getOperationRelativeTime(),
        endTime: this.getOperationRelativeTime(),
        delayDuration: this.getLastChangePause(),
        ops: changes,
        combo: 1
      });
    }
    swapDocListener(editor, oldDoc) {
      const changes = [{
        from: { line: 0, ch: 0 },
        to: {
          line: oldDoc.lastLine(),
          ch: oldDoc.getLine(oldDoc.lastLine()).length
        },
        origin: "setValue",
        removed: oldDoc.getValue().split("\n"),
        text: editor.getValue().split("\n")
      }];
      this.operations.push({
        startTime: this.getOperationRelativeTime(),
        endTime: this.getOperationRelativeTime(),
        delayDuration: this.getLastChangePause(),
        ops: changes,
        combo: 1
      });
    }
    cursorActivityListener(editor) {
      this.operations.push({
        startTime: this.getOperationRelativeTime(),
        endTime: this.getOperationRelativeTime(),
        delayDuration: this.getLastCursorActivityPause(),
        crs: editor.listSelections(),
        combo: 1
      });
    }
    isPasteOperation(operation) {
      for (let i = 0; i < operation.ops.length; i++) {
        if (operation.ops[i].origin === "paste") {
          return true;
        }
      }
      return false;
    }
    removeRedundantCursorOperations() {
      const operations = this.operations;
      const newOperations = [];
      for (let i = 0; i < operations.length; i++) {
        if ("ops" in operations[i]) {
          newOperations.push(operations[i]);
          if (i > 0 && this.isPasteOperation(operations[i])) {
            operations[i - 1].startTime = operations[i].startTime + 1;
            operations[i - 1].endTime = operations[i].endTime + 1;
            newOperations.push(operations[i - 1]);
          }
        } else if (!(i < operations.length - 1 && "ops" in operations[i + 1])) {
          newOperations.push(operations[i]);
        }
      }
      this.operations = newOperations;
    }
    compressCursorOperations() {
      let operations = this.operations;
      operations = compress_default.select(operations);
      operations = compress_default.cursor(operations);
      this.operations = operations;
    }
    compressChanges() {
      let operations = this.operations;
      operations = compress_default.input(operations);
      operations = compress_default.remove(operations);
      operations = compress_default.compose(operations);
      this.operations = operations;
    }
  };

  // codemirror-record/src/func/extract/compose.js
  function compose_default2(op, i) {
    const startTime = op.t[0];
    const durationPerOperation = (op.t[1] - op.t[0]) / (op.l - 1);
    const composition = { t: null, o: [], type: "content" };
    composition.t = Math.floor(startTime + i * durationPerOperation);
    if (i === op.l - 1) {
      composition.t = op.t[1];
    }
    const cursorsPos = [];
    for (let j = 0; j < op.o.length; j++) {
      cursorsPos.push(op.o[j].i);
      composition.o.push({ a: null, i: null });
    }
    for (let j = 0; j < op.o.length; j++) {
      composition.o[j].a = op.o[j].a[i];
      composition.o[j].i = [cursorsPos[j][0], cursorsPos[j][1]];
      cursorsPos[j][1] += op.o[j].a[i].length;
    }
    return composition;
  }

  // codemirror-record/src/func/extract/cursor.js
  function cursor_default2(op, i) {
    const startTime = op.t[0];
    const durationPerOperation = (op.t[1] - op.t[0]) / (op.l - 1);
    const cursor = { t: null, o: [], type: "cursor" };
    cursor.t = Math.floor(startTime + i * durationPerOperation);
    if (i === op.l - 1) {
      cursor.t = op.t[1];
    }
    const cursorsPos = [];
    for (let j = 0; j < op.o.length; j++) {
      if (!Array.isArray(op.o[j].i[0])) {
        op.o[j].i = [op.o[j].i, op.o[j].i];
      }
      cursorsPos.push(op.o[j].i);
      cursor.o.push({ i: null });
    }
    for (let j = 0; j < op.o.length; j++) {
      const posLine = cursorsPos[j][0][0];
      const posCh = cursorsPos[j][0][1] + (cursorsPos[j][1][1] - cursorsPos[j][0][1]) / (op.l - 1) * i;
      cursor.o[j].i = [posLine, posCh];
    }
    return cursor;
  }

  // codemirror-record/src/func/extract/input.js
  function input_default2(op, i) {
    const startTime = op.t[0];
    const durationPerOperation = (op.t[1] - op.t[0]) / (op.l - 1);
    const insertion = { t: null, o: [], type: "content" };
    insertion.t = Math.floor(startTime + i * durationPerOperation);
    if (i === op.l - 1) {
      insertion.t = op.t[1];
    }
    const cursorsPos = [];
    for (let j = 0; j < op.o.length; j++) {
      cursorsPos.push(op.o[j].i);
      insertion.o.push({ a: null, i: null });
    }
    for (let j = 0; j < op.o.length; j++) {
      insertion.o[j].a = op.o[j].a[i];
      insertion.o[j].i = [cursorsPos[j][0], cursorsPos[j][1]];
      if (insertion.o[j].a !== "\n") {
        cursorsPos[j][1]++;
      } else {
        cursorsPos[j][0]++;
        cursorsPos[j][1] = 0;
      }
    }
    return insertion;
  }

  // codemirror-record/src/func/extract/select.js
  function extractToPositions(toPos) {
    const toPositions = [];
    for (let i = 0; i < toPos.length; i++) {
      for (let j = 0; j < toPos[i][1].length; j++) {
        if (typeof toPos[i][1][j] === "number") {
          toPositions.push([toPos[i][0], toPos[i][1][j]]);
        } else {
          const direction = toPos[i][1][j][0] < toPos[i][1][j][1] ? 1 : -1;
          let ch = toPos[i][1][j][0];
          toPositions.push([toPos[i][0], ch]);
          while (ch !== toPos[i][1][j][1]) {
            ch += direction;
            toPositions.push([toPos[i][0], ch]);
          }
        }
      }
    }
    return toPositions;
  }
  function select_default2(op, i) {
    const startTime = op.t[0];
    const durationPerOperation = (op.t[1] - op.t[0]) / (op.l - 1);
    const selection = { t: null, o: [], type: "cursor" };
    selection.t = Math.floor(startTime + i * durationPerOperation);
    if (i === op.l - 1) {
      selection.t = op.t[1];
    }
    const cursorsPos = [];
    for (let j = 0; j < op.o.length; j++) {
      cursorsPos.push(op.o[j].i);
      selection.o.push({ i: null });
    }
    for (let j = 0; j < op.o.length; j++) {
      const fromPos = [
        op.o[j].i[0],
        op.o[j].i[1]
      ];
      const toPositions = extractToPositions(op.o[j].s);
      const toPos = [
        toPositions[i][0],
        toPositions[i][1]
      ];
      selection.o[j].i = [fromPos, toPos];
    }
    return selection;
  }

  // codemirror-record/src/func/extract/remove.js
  function remove_default2(op, i) {
    const startTime = op.t[0];
    const durationPerOperation = (op.t[1] - op.t[0]) / (op.l - 1);
    const deletion = { t: null, o: [], type: "content" };
    deletion.t = Math.floor(startTime + i * durationPerOperation);
    if (i === op.l - 1) {
      deletion.t = op.t[1];
    }
    const cursorsPos = [];
    for (let j = 0; j < op.o.length; j++) {
      cursorsPos.push(op.o[j].i[1]);
      deletion.o.push({ i: null });
    }
    for (let j = 0; j < op.o.length; j++) {
      const currentDeletion = op.o[j].d.pop();
      if (typeof currentDeletion[0] === "number") {
        deletion.o[j].i = [
          [
            cursorsPos[j][0],
            cursorsPos[j][1] - currentDeletion[0]
          ],
          [
            cursorsPos[j][0],
            cursorsPos[j][1]
          ]
        ];
        cursorsPos[j][1] -= currentDeletion[0];
        if (currentDeletion[1] - 1 > 0) {
          op.o[j].d.push([currentDeletion[0], currentDeletion[1] - 1]);
        }
      } else {
        deletion.o[j].i = [
          [
            currentDeletion[0][0],
            currentDeletion[0][1]
          ],
          [
            currentDeletion[1][0],
            currentDeletion[1][1]
          ]
        ];
        op.o[j].i[1] = [
          currentDeletion[0][0],
          currentDeletion[0][1]
        ];
      }
    }
    return deletion;
  }

  // codemirror-record/src/func/extract/index.js
  var extract_default = {
    compose: compose_default2,
    cursor: cursor_default2,
    input: input_default2,
    select: select_default2,
    remove: remove_default2
  };

  // codemirror-record/src/CodePlay.js
  var CodePlay = class {
    constructor(editor, options) {
      this.editor = editor;
      this.initialize();
      if (options) {
        this.maxDelay = options.maxDelay || config_default.maxDelayBetweenOperations;
        this.autoplay = options.autoplay || false;
        this.speed = options.speed || 1;
        this.extraActivityHandler = options.extraActivityHandler || null;
        this.extraActivityReverter = options.extraActivityReverter || null;
      }
    }
    initialize() {
      this.operations = [];
      this.playedOperations = [];
      this.cachedValue = null;
      this.status = "PAUSE";
      this.timer = null;
      this.currentOperation = null;
      this.duration = 0;
      this.lastOperationTime = 0;
      this.lastPlayTime = 0;
      this.seekTime = null;
      this.playedTimeBeforeOperation = 0;
      this.playedTimeBeforePause = 0;
      this.speedBeforeSeeking = null;
    }
    setOption(setOptionCallback) {
      const statusBeforeSetOption = this.status;
      if (statusBeforeSetOption === "PLAY") {
        this.pause();
      }
      setOptionCallback();
      if (statusBeforeSetOption === "PLAY") {
        this.play();
      }
    }
    setMaxDelay(maxDelay) {
      this.setOption(() => {
        if (maxDelay) {
          this.maxDelay = maxDelay;
        }
      });
    }
    setAutoplay(autoplay) {
      this.setOption(() => {
        if (autoplay) {
          this.autoplay = autoplay;
        }
      });
    }
    setSpeed(speed) {
      this.setOption(() => {
        if (speed) {
          this.speed = speed;
        }
      });
    }
    setExtraActivityHandler(extraActivityHandler) {
      this.setOption(() => {
        if (extraActivityHandler) {
          this.extraActivityHandler = extraActivityHandler;
        }
      });
    }
    setExtraActivityReverter(extraActivityReverter) {
      this.setOption(() => {
        if (extraActivityReverter) {
          this.extraActivityReverter = extraActivityReverter;
        }
      });
    }
    addOperations(operations) {
      const parsedOperations = this.parseOperations(operations);
      this.operations = this.operations.concat(parsedOperations);
      this.duration = parsedOperations[parsedOperations.length - 1].t;
      if (this.autoplay) {
        this.play();
      }
    }
    clear() {
      this.initialize();
    }
    isAutoIndent(operationOp) {
      return operationOp.o === "i" && operationOp.a === "";
    }
    play() {
      if (this.status !== "PLAY") {
        this.editor.focus();
        this.playChanges();
      }
    }
    pause() {
      if (this.status !== "PAUSE") {
        this.status = "PAUSE";
        this.playedTimeBeforePause = (new Date().getTime() - this.lastPlayTime) * this.speed;
        this.playedTimeBeforeOperation += this.playedTimeBeforePause;
        if (this.currentOperation !== null) {
          clearTimeout(this.timer);
          this.currentOperation = null;
        }
      }
    }
    getStatus() {
      return this.status;
    }
    getCurrentTime() {
      const currentTime = this.lastOperationTime + this.playedTimeBeforeOperation;
      if (this.status === "PLAY") {
        const timeAfterLastPlay = (new Date().getTime() - this.lastPlayTime) * this.speed;
        return currentTime + timeAfterLastPlay;
      }
      return currentTime;
    }
    getDuration() {
      return this.duration;
    }
    seek(seekTime) {
      this.speedBeforeSeeking = this.speed;
      this.statusBeforeSeeking = this.status;
      this.speed = 0;
      this.seekTime = seekTime;
      this.editor.focus();
      this.pause();
      if (this.lastOperationTime < this.seekTime) {
        this.playChanges();
      } else if (this.lastOperationTime > this.seekTime) {
        this.revertChanges();
      }
    }
    stopSeek() {
      this.pause();
      if (this.seekTime) {
        this.playedTimeBeforeOperation = this.seekTime - this.lastOperationTime;
        if (this.speedBeforeSeeking !== null) {
          this.setSpeed(this.speedBeforeSeeking);
        }
        this.seekTime = null;
        if (this.statusBeforeSeeking === "PLAY") {
          this.play();
        }
      }
    }
    playChanges() {
      this.lastPlayTime = new Date().getTime();
      const operations = this.operations;
      if (operations.length > 0) {
        this.status = "PLAY";
        this.currentOperation = operations[0];
        const currentOperation = this.currentOperation;
        const currentOperationDelay = this.getOperationDelay(currentOperation);
        if (this.seekTime && currentOperation.t > this.seekTime) {
          this.stopSeek();
          return;
        }
        this.timer = setTimeout(() => {
          this.lastOperationTime = currentOperation.t;
          this.operations.shift();
          this.playChange(this.editor, currentOperation);
          if (this.operations.length === 0) {
            this.currentOperation = null;
            this.stopSeek();
          }
        }, this.speed === 0 ? 0 : currentOperationDelay / this.speed);
      }
    }
    getOperationDelay(currentOperation) {
      const operationDelay = currentOperation.t - this.lastOperationTime;
      const realOperationDelay = operationDelay - this.playedTimeBeforeOperation;
      if (realOperationDelay > this.maxDelay && this.maxDelay > 0) {
        return this.maxDelay;
      }
      return realOperationDelay;
    }
    playChange(editor, currentOperation) {
      this.playedTimeBeforeOperation = 0;
      const valueBeforeChange = editor.getValue();
      if (this.cachedValue === null || this.cachedValue !== valueBeforeChange) {
        this.cachedValue = valueBeforeChange;
        currentOperation.revertValue = valueBeforeChange;
      }
      for (let i = 0; i < currentOperation.o.length; i++) {
        if (this.playExtraActivity(currentOperation)) {
          break;
        }
        const insertContent = this.insertionText(currentOperation.o[i]);
        let insertPos = currentOperation.o[i].i;
        if (typeof insertPos[0] === "number") {
          insertPos = [insertPos, insertPos];
        }
        if (!this.isAutoIndent(currentOperation.o[i])) {
          if (currentOperation.o[0].a !== "\n\n") {
            if (i === 0) {
              editor.setSelection({ line: insertPos[0][0], ch: insertPos[0][1] }, { line: insertPos[1][0], ch: insertPos[1][1] });
            } else {
              editor.addSelection({ line: insertPos[0][0], ch: insertPos[0][1] }, { line: insertPos[1][0], ch: insertPos[1][1] });
            }
          }
        }
        if (currentOperation.type === "content") {
          editor.replaceRange(insertContent, { line: insertPos[0][0], ch: insertPos[0][1] }, { line: insertPos[1][0], ch: insertPos[1][1] });
        }
      }
      this.playedOperations.unshift(currentOperation);
      this.playChanges();
    }
    playExtraActivity(currentOperation) {
      if (currentOperation.type === "extra") {
        if (this.extraActivityHandler) {
          this.extraActivityHandler(currentOperation.o[0].activity);
        } else {
          console.warn("extraActivityHandler is required in player");
        }
        return true;
      }
      return false;
    }
    insertionText(cursorOperation) {
      let insertContent = "";
      if (typeof cursorOperation.a === "string") {
        insertContent = cursorOperation.a;
      } else if ("a" in cursorOperation) {
        insertContent = cursorOperation.a.join("\n");
      }
      return insertContent;
    }
    revertChanges() {
      const playedOperations = this.playedOperations;
      if (playedOperations.length > 0) {
        this.currentOperation = playedOperations[0];
        this.revertChange(this.editor, this.currentOperation);
      } else {
        this.lastOperationTime = 0;
        this.stopSeek();
        return;
      }
    }
    revertChange(editor, currentOperation) {
      this.lastOperationTime = currentOperation.t;
      if (this.seekTime && this.lastOperationTime <= this.seekTime) {
        this.stopSeek();
        return;
      }
      if (currentOperation.revertValue !== void 0) {
        editor.setValue(currentOperation.revertValue);
      }
      this.revertExtraActivity(currentOperation);
      this.playedOperations.shift();
      this.operations.unshift(currentOperation);
      this.revertChanges();
    }
    revertExtraActivity(currentOperation) {
      if (currentOperation.type === "extra") {
        if (this.extraActivityReverter) {
          this.extraActivityReverter(currentOperation.o[0].activity);
        } else {
          console.warn("extraActivityReverter is required in player");
        }
        return true;
      }
      return false;
    }
    classifyOperation(operation) {
      operation.type = "content";
      if (operation.o[0].o === "o" || operation.o[0].o === "l") {
        operation.type = "cursor";
      } else if (operation.o[0].o === "e") {
        operation.type = "extra";
      }
      return operation;
    }
    parseOperations(operations) {
      operations = JSON.parse(operations);
      const extractedOperations = [];
      for (let operation of operations) {
        operation = this.classifyOperation(operation);
        if ("l" in operation) {
          for (let i = 0; i < operation.l; i++) {
            if (operation.o[0].o === "i") {
              extractedOperations.push(extract_default.input(operation, i));
            } else if (operation.o[0].o === "c") {
              extractedOperations.push(extract_default.compose(operation, i));
            } else if (operation.o[0].o === "d") {
              extractedOperations.push(extract_default.remove(operation, i));
            } else if (operation.o[0].o === "o") {
              extractedOperations.push(extract_default.cursor(operation, i));
            } else if (operation.o[0].o === "l") {
              extractedOperations.push(extract_default.select(operation, i));
            }
          }
        } else {
          extractedOperations.push(operation);
        }
      }
      return extractedOperations;
    }
  };

  // app.jsx

  var codeRecorder = new CodeRecord(controlEditor);
  
  controlEditor.on("keyHandled", function (instance, name, event){
  // you may use the key described in the event parameter
  codeRecorder.recordExtraActivity(name) // let us record the key combination here
})
  
  var codePlayer = new CodePlay(controlEditor, {
    autoplay: true,
    extraActivityHandler: (name) => {
   	

    	switch(name) {
    		case "Ctrl-.":
    			controlEditor.execCommand('hush');
    			break;
    		case "Ctrl-1":
    			muteP1();
    			break;
    		case "Ctrl-S":
    			controlEditor.execCommand("controlSaveFile");
    			break;
    	}

  },
    extraActivityReverter: (name) => {
    	controlEditor.getOption("extraKeys")[name].call;
  }	
  });

