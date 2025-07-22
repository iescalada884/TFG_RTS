#!/bin/bash

# Configuration
GIT_REPO_URL="https://github.com/iescalada884/TFG_RTS"
ASSET_EXTENSION=".tar.gz"

# Derived variables
API_URL="${GIT_REPO_URL/github.com/api.github.com\/repos}"
REPO_NAME=$(basename "$GIT_REPO_URL")
MARTE_FOLDER_NAME="marte_28mar2023"
DEFAULT_INSTALL_DIR="."
MARTE_INSTALL_NAME="marte"
DEFAULT_MARTE_DIR="$(pwd)/$MARTE_INSTALL_NAME"

# Functions
show_help() {
    echo "Usage: $0 [OPTIONS] [VERSION] [INSTALL_DIR]"
    echo ""
    echo "Install MARTE real-time system from GitHub releases"
    echo ""
    echo "OPTIONS:"
    echo "  --list              List available versions"
    echo "  --install [DIR]     Install MARTE from existing marte directory (default: ./$MARTE_INSTALL_NAMEmarte)"
    echo "  --help, -h          Show this help message"
    echo "  --marte_dir_name, -o <name>  Specify custom MARTE directory name (default: $MARTE_INSTALL_NAME)"
    echo ""
    echo "ARGUMENTS:"
    echo "  VERSION             Version to install (default: latest)"
    echo "  INSTALL_DIR         Directory to install MARTE (default: current directory)"
    echo ""
    echo "Examples:"
    echo "  $0                           # Install latest version into current directory"
    echo "  $0 v1.0.0                   # Install v1.0.0 to current directory"
    echo "  $0 latest /opt/marte         # Install latest version to /opt/marte"
    echo "  $0 v1.0.0 ~/projects        # Install v1.0.0 to ~/projects"
    echo "  $0 --install                 # Install MARTE in ./marte directory"
    echo "  $0 --install /path/to/marte  # Install MARTE in specified directory"
    echo ""
}

list_versions() {
    echo "Fetching available versions for $REPO_NAME..."

    # Get the latest release tag
    LATEST_TAG=$(curl -s "$API_URL/releases/latest" | jq -r '.tag_name')

    # Fetch and display all releases
    curl -s "$API_URL/releases" | jq -r --arg LATEST "$LATEST_TAG" '
        .[] |
        . as $r |
        ($r.tag_name + 
         (if $r.tag_name == $LATEST then " [latest]" else "" end) +
         (if $r.prerelease == true then " [pre-release]" else "" end)
        ) + ": " + ($r.name // "No name") + 
        "\n" + ($r.body // "No changelog provided.") + "\n---"
    '

    # Notify if the latest tag wasn't found (edge case)
    if ! curl -s "$API_URL/releases" | jq -r '.[] | .tag_name' | grep -q "^$LATEST_TAG$"; then
        echo "[Notice] Latest release ($LATEST_TAG) is not listed in the standard releases (possibly a draft or pre-release)."
    fi

    exit 0
}

install_marte() {
    PROJECT_ROOT=$1

    # Add utils folder to PATH if not already added
    if [[ ":$PATH:" != *":$PROJECT_ROOT/utils:"* ]]; then
        export PATH="$PROJECT_ROOT/utils:$PATH"
        echo "Added $PROJECT_ROOT/utils to PATH"
    fi

    cd "$PROJECT_ROOT" || {
        echo "Failed to enter project root directory: $PROJECT_ROOT"
        return 1
    }
    pwd
    # Execute ./minstall
    if [ -x ./minstall ]; then
        echo "Running ./minstall..."
        ./minstall < "13"
    else
        echo "Error: ./minstall not found or not executable in $PROJECT_ROOT"
        return 1
    fi

    # Select architecture
    echo "Selecting architecture: stm32 f4"
    msetcurrentarch stm32f f4
    # Check if msetcurrentarch was successful
    if [ $? -ne 0 ]; then
        echo "Error: Failed to set current architecture."
        exit 1
    fi

    # Compile RTS and Marte
    echo "Compiling RTS and Marte..."
    mkrtsmarteuc
    if [ $? -ne 0 ]; then
        echo "Error: Compilation of RTS failed. Check the output for details."
        exit 1
    fi

    mkmarte
    if [ $? -ne 0 ]; then
        echo "Error: Compilation of Marte failed. Check the output for details."
        exit 1
    fi

    echo "Marte installation complete."

    echo "To use Marte commands, you may need to add the utils directory to your PATH:"
    echo "export PATH=\"\$PATH:$PROJECT_ROOT/utils\""
    echo "Or you can run Marte commands directly from the utils directory."
}


install_repo_version() {
    VERSION="$1"
    INSTALL_DIR="${2:-$DEFAULT_INSTALL_DIR}"

    # Convert relative path to absolute path
    INSTALL_DIR=$(realpath "$INSTALL_DIR")

    echo INSTALL_DIR: "$INSTALL_DIR"

    # Ensure install directory exists
    if [ ! -d "$INSTALL_DIR" ]; then
        echo "Creating install directory: $INSTALL_DIR"
        mkdir -p "$INSTALL_DIR" || {
            echo "Error: Failed to create install directory: $INSTALL_DIR"
            exit 1
        }
    fi

    if [ "$VERSION" = "latest" ]; then
        echo "Fetching latest stable release..."
        VERSION=$(curl -s "$API_URL/releases/latest" | jq -r '.tag_name')

        if [ "$VERSION" = "null" ] || [ -z "$VERSION" ]; then
            echo "No stable release found. Falling back to latest pre-release..."
            VERSION=$(curl -s "$API_URL/releases" | jq -r '[.[] | select(.prerelease == true)][0].tag_name')

            if [ -z "$VERSION" ] || [ "$VERSION" = "null" ]; then
                echo "Error: No releases (stable or pre-release) are available."
                exit 1
            fi

            echo "Installing pre-release version: $VERSION"
        else
            echo "Installing version: $VERSION"
        fi
    fi

    ARCHIVE_URL="$GIT_REPO_URL/archive/refs/tags/${VERSION}.zip"
    ARCHIVE_NAME="/tmp/${REPO_NAME}-${VERSION}-source.zip"
    EXTRACT_DIR="/tmp/${REPO_NAME}-${VERSION#v}"

    echo "Downloading source archive from: $ARCHIVE_URL"
    curl -L -o "$ARCHIVE_NAME" "$ARCHIVE_URL"

    echo "Extracting archive to $EXTRACT_DIR ..."
    unzip -q "$ARCHIVE_NAME" -d /tmp

    echo "Removing archive $ARCHIVE_NAME"
    rm -f "$ARCHIVE_NAME"

    cd "$EXTRACT_DIR" 2>/dev/null || {
        echo "Failed to enter extracted directory: $EXTRACT_DIR"
        exit 1
    }

    echo "Setting up marte folders..."

    # Check if MARTE folder exists in the extracted archive
    if [ -d "$MARTE_FOLDER_NAME" ]; then
        echo "Copying $MARTE_FOLDER_NAME to $INSTALL_DIR/$MARTE_INSTALL_NAME ..."
        cp -r "$MARTE_FOLDER_NAME" "$INSTALL_DIR/$MARTE_INSTALL_NAME" || {
            echo "Error: Failed to copy $MARTE_FOLDER_NAME to $INSTALL_DIR/$MARTE_INSTALL_NAME"
            exit 1
        }
        echo "MARTE successfully installed to: $INSTALL_DIR/$MARTE_INSTALL_NAME"
    else
        echo "Warning: $MARTE_FOLDER_NAME folder not found in the archive."
        echo "Available folders in the archive:"
        ls -la
        echo "Copying entire archive contents to $INSTALL_DIR ..."
        cp -r . "$INSTALL_DIR/" || {
            echo "Error: Failed to copy archive contents to $INSTALL_DIR"
            exit 1
        }
        echo "Archive contents copied to: $INSTALL_DIR"
    fi

    #install_marte "$INSTALL_DIR"
    echo "Running Marte installation steps in $INSTALL_DIR/$MARTE_INSTALL_NAME..."
    install_marte "$INSTALL_DIR/$MARTE_INSTALL_NAME"

    # Clean up temporary extraction directory
    cd /tmp
    rm -rf "$EXTRACT_DIR"

    echo "Installation complete."
}

#Process parameter flags
case "$1" in
    --marte_dir_name|-o)
        if [ -z "$2" ]; then
            echo "Error: --marte_dir_name requires a directory name."
            exit 1
        fi
        MARTE_INSTALL_NAME="$2"
        echo "Using custom MARTE directory name: $MARTE_INSTALL_NAME"
        shift 2
        ;;
esac

case "$1" in
    -l|--list)
        list_versions
        exit 0
        ;;
    --install)
        TARGET_DIR="${2:-$DEFAULT_MARTE_DIR}"
        echo "Running install_marte on: $TARGET_DIR"

        if [ ! -d "$TARGET_DIR" ]; then
            echo "Error: Target directory '$TARGET_DIR' does not exist."
            echo "Please run the repo installation first or check the path."
            exit 1
        fi

        install_marte "$TARGET_DIR" || {
            echo "install_marte failed."
            exit 1
        }
        ;;
    --help|-h)
        show_help
        exit 0
        ;;
    -*|--*)
        echo "Error: Unknown option '$1'. Use --help for usage information."
        exit 1
        ;;
    *)
        echo "Installing MARTE version: ${1:-latest} to directory: ${2:-$DEFAULT_INSTALL_DIR}"
        install_repo_version "${1:-latest}" "$2"
        ;;
esac