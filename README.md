# my-hakyll

## Usage

- Set proxy

    ```pwsh
    $env:HTTPS_PROXY = "http://127.0.0.1:2082"
    ```
- Build

    ```
    stack build
    ```

    Append `--verbose` for troubleshooting.
- Watch

    ```
    stack exec site watch
    ```
