# Spectrum Finance Vesting with period protocol

The Vesting with Period Protocol enables individuals to create their own customized vesting programs.

### Datum (Vesting configuration)

| Field                   | Type                | Description                         |
| ----------------------- | ------------------- | ----------------------------------- |
| `vestingStart`          | `POSIXTime`         | Time of program start               |
| `vestingPeriodDuration` | `POSIXTime`         | Period duration                     |
| `totalVested`           | `Integer`           | Program budget                      |
| `totalVested`           | `Integer`           | Period budget                       |
| `pkhs`                  | `List[PPubKeyHash]` | List of authority members           |
| `vestingAC`             | `AssetClass`        | Program budget token (e.g ADA, SPF) |

### Redeemer (Withdraw part)

| Field              | Type      | Description                              |
| ------------------ | --------- | ---------------------------------------- |
| `vestingInIx`      | `Integer` | Index of box with vesting program budget |
| `vestingPeriodIdx` | `Integer` | Index of period, that should be unlocked |

### Tokens

| Name                 | Description                                           |
| -------------------- | ----------------------------------------------------- |
| Program budget token | Token that will be distributed during vesting program |

## User scenarios

### Vesting program creation 
Alice implemented a vesting program (Program X) and aims to incentivize workers. She introduces a vesting program for a duration of 1 year with a 3-month unlock period. To achieve this, Alice initializes the following parameters: vestingStart to the current time, vestingPeriodDuration set as 3 months, totalVested representing the program budget, pkhs denoting the list of workers' public keys, and vestingAC as the AssetClass of token Xt. Subsequently, Alice sends totalVested tokens Xt along with the corresponding datum to the address of the "Vesting with period" script.

### Vesting program withdraw
To retrieve tokens from the vesting program, users should wait until the specified period ends. The period duration is calculated as (vestingStart + periodId * vestingPeriodDuration). Once the period has ended, users can interact with the vesting program box by creating a transaction. The transaction should include the following parameters: vestingInIx (representing the index of the vesting box in the inputs list) and vestingPeriodIdx (representing the period ID) in redeemer.
In addition, the transaction should be signed by all private keys corresponding to the public key hashes from pkhs.