---
title: "Globus CLI"
type: docs
toc: true
date: 2023-02-02T00:00:00-05:00
weight: 3080
menu:
    globus
---

Globus CLI is a command line wrapper over the Globus SDK for Python, which provides an interface to Globus services from the shell, and is suited to both interactive and simple scripting use cases.

## Installation

Instructions for installing Globus CLI on your local compute can be found through the [official Globus documentation](https://docs.globus.org/cli/#installation). Globus CLI can be installed either with `pip` or `pipx`. Globus CLI is also available on Afton/Rivanna as an Lmod module and can be loaded with

```module load gcc globus-cli``` 

Ensure that [Globus Connect Personal](/notes/globus-data-transfer/installation) is installed on your local computer and a [personal collection](/notes/globus-data-transfer/setup) is created. This is reqired to serve as the source endpoint.

## Authentication

Run `globus login` to log into Globus for Globus CLI credentials. Following this, run `globus session update --all` to update your active Globus CLI session.

## Endpoint Configuration

**Source Endpoint**

Run `globus endpoint local-id` on the command line to obtain your local endpoint ID.

**Destination Endpoint**

The Destination endpoint for UVA Standard Security Storage is `af187d15-768f-4449-8670-d00e1eb1ce6a`

<div role="note" style="background-color: #dc3545; border-left: 4px solid #2196F3; padding: 12px; margin: 16px 0;">
  <strong>Note:</strong> Destination folders are user-specific and should not be hardcoded. Each researcher will have a unique destination path.
</div>


## Sync Strategies

Below is a key command to perform recursive folder transfers:

```globus transfer --recursive --sync-level checksum SOURCE DEST```

**Recursive Transfer**
The `--recursive` flag ensures:
* Nested folders transfer correctly
* Folder structure is preserved
* All files inside subdirectories are included

**Sync-Level Options**

| Option | Purpose |
| :--- | :--- |
| `exists` | Skip files that already exist at destination |
| `size` | Compare files by file size |
| `mtime` | Compare files by modification time |
| `checksum` | Compare actual file content (strongest guarantee) |

**Recommended Default:** `checksum`

Checksum is recommended because:
* It provides the strongest correctness guarantee
* It avoids timestamp mismatch issues common across systems
* It is best suited for research data integrity requirements

**Alternative — `mtime`:** Useful when speed matters more than strict verification and timestamps are reliable across systems.

## Matlab Example

MATLAB's `system()` function can be used to call Globus CLI commands.

Download {{< file-download file="/notes/globus-data-transfer/scripts/matlab-globus.m" text="matlab-globus.m" >}} for a simple template on how to configure a matlab script to transfer files automatically with Globus CLI.

The Matlab script sets source and destination endpoints and paths along with a filename and the full file path to the globus executible. The script then generates and saves a png plot. Finally, the `globus transfer` command is setup prior to calling the `system()` function to execute the transfer.

<div role="note" style="background-color: #dc3545; border-left: 4px solid #2196F3; padding: 12px; margin: 16px 0;">
  <strong>Note:</strong> Destination directories should not begin with a forward slash (/). The destination path needs to be relative since it's a Globus Connect Server v5 Mapped Collection endpoint.
</div>

## Considerations and Error Handling

**Authentication Expiry**

rerun:
```
globus login
```

**Missing Session Consent**

rerun:
```
globus session update --all
```

**Permission Denied**

Usually indicates a destination write access issue. Verify endpoint permissions.

**File Not Found**

Usually caused by an incorrect source path. Verify the local path before running.

**Duplicate Transfer Conflict (409 Conflict)**

Occurs when an identical transfer is already running.  
**Resolution:**
```bash
globus task list
globus task cancel <TASK_ID>
```
### Important Constraint
**Full reauthentication cannot be safely automated.** Globus intentionally requires user-mediated consent for protected resources.

You may periodically need to:
* Approve browser login
* Complete session consent
* Reauthenticate when tokens expire
