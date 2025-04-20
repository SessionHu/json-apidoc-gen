# JSON APIDoc Generator (json-apidoc-gen)

[![Haskell](https://img.shields.io/badge/Language-Haskell-purple.svg?style=flat-square)](https://www.haskell.org/)

**JSON APIDoc Generator** is a simple command-line tool written in Haskell for generating BAC document templates from JSON inputs.

## Features

- **Automatic JSON Parsing**: Reads and parses JSON data from standard input.
- **Flexible Type Inference**: Detects field types in JSON, including arrays and nested objects.
- **Markdown Documentation Output**: Outputs documentation templates in a human-readable Markdown format.
- **Designed for API Documentation**: Ideal for generating API design documents or interface descriptions.
- **BAC Based**: Originally designed for [bilibili-API-collect](https://github.com/SocialSisterYi/bilibili-API-collect)

## Installation

### Build from Source

**Dependencies:**
- GHC (Glasgow Haskell Compiler)
- [Cabal](https://www.haskell.org/cabal/) (for building and managing Haskell projects)

**Steps:**

1. Clone the repository:
   ```bash
   git clone https://github.com/SessionHu/json-apidoc-gen.git
   cd json-apidoc-gen
   ```

2. Build the project using Cabal:

   ```bash
   cabal build
   ```

3. Run the CLI tool:

   ```bash
   cabal run
   ```

## Usage

### JSON Input

The tool reads JSON data from the standard input. You can provide input in the following ways:
- Pipe input: `echo '{"key": "value"}' | cabal run`
- Read from a file: `cat example.json | cabal run`
- Input manually from *stdin*

### Command Output

The tool generates a documentation template in Markdown format and outputs it to the standard output. Usually you can pipe or redirect it to another file. The typical structure includes:
- Basic information (title, URI Scheme, HTTP method, etc.)
- Field descriptions for the JSON data
- Example response data

### Example

Given the following JSON data:

```json
{
  "code": 0,
  "msg": "0",
  "ttl": 1,
  "data": {
    "hello": "world",
    "bili": [
      "qwq",
      "awa",
      "sess"
    ],
    "count": 114514
  }
}
```

The tool generates the following documentation template:

````markdown
## title

> https://

*请求方法: *

认证方式:

**URL 参数:**

**JSON 回复:**

根对象:

| 字段 | 类型 | 内容 | 备注 |
| ---- | ---- | ---- | ---- |
| code | number |  |  |
| data | object |  |  |
| msg | string |  |  |
| ttl | number |  |  |

`data` 对象:

| 字段 | 类型 | 内容 | 备注 |
| ---- | ---- | ---- | ---- |
| bili | string[] |  |  |
| count | number |  |  |
| hello | string |  |  |

**示例:**

```shell
curl -
```

<details>
<summary>查看响应示例:</summary>

```json
{
  "code": 0,
  "data": {
    "bili": [
      "qwq",
      "awa",
      "sess"
    ],
    "count": 114514,
    "hello": "world"
  },
  "msg": "0",
  "ttl": 1
}
```
</details>
````

### Error Handling

- If the input JSON is invalid, the tool outputs an error message to the standard error.
- If the root of the JSON is not an object, the tool will indicate the unsupported type.

### Known Issues

- No i18n support, only support output in Simplified Chinese.
- No object structure check within array.
- Unrecognized object to be used as Map.

## License

This project is licensed under the [GNU General Public License v3.0](LICENSE).

---

Thank you for using JSON APIDoc Generator! If you encounter any issues or have suggestions for improvement, feel free to submit an [Issue](https://github.com/SessionHu/json-apidoc-gen/issues).
