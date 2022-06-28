pragma solidity >=0.6.0 <0.8.0;


// SPDX-License-Identifier: MIT
/**
 * @dev Wrappers over Solidity's arithmetic operations with added overflow
 * checks.
 *
 * Arithmetic operations in Solidity wrap on overflow. This can easily result
 * in bugs, because programmers usually assume that an overflow raises an
 * error, which is the standard behavior in high level programming languages.
 * `SafeMath` restores this intuition by reverting the transaction when an
 * operation overflows.
 *
 * Using this library instead of the unchecked operations eliminates an entire
 * class of bugs, so it's recommended to use it always.
 */
library SafeMathUpgradeable {
    /**
     * @dev Returns the addition of two unsigned integers, with an overflow flag.
     *
     * _Available since v3.4._
     */
    function tryAdd(uint256 a, uint256 b) internal pure returns (bool, uint256) {
        uint256 c = a + b;
        if (c < a) return (false, 0);
        return (true, c);
    }

    /**
     * @dev Returns the substraction of two unsigned integers, with an overflow flag.
     *
     * _Available since v3.4._
     */
    function trySub(uint256 a, uint256 b) internal pure returns (bool, uint256) {
        if (b > a) return (false, 0);
        return (true, a - b);
    }

    /**
     * @dev Returns the multiplication of two unsigned integers, with an overflow flag.
     *
     * _Available since v3.4._
     */
    function tryMul(uint256 a, uint256 b) internal pure returns (bool, uint256) {
        // Gas optimization: this is cheaper than requiring 'a' not being zero, but the
        // benefit is lost if 'b' is also tested.
        // See: https://github.com/OpenZeppelin/openzeppelin-contracts/pull/522
        if (a == 0) return (true, 0);
        uint256 c = a * b;
        if (c / a != b) return (false, 0);
        return (true, c);
    }

    /**
     * @dev Returns the division of two unsigned integers, with a division by zero flag.
     *
     * _Available since v3.4._
     */
    function tryDiv(uint256 a, uint256 b) internal pure returns (bool, uint256) {
        if (b == 0) return (false, 0);
        return (true, a / b);
    }

    /**
     * @dev Returns the remainder of dividing two unsigned integers, with a division by zero flag.
     *
     * _Available since v3.4._
     */
    function tryMod(uint256 a, uint256 b) internal pure returns (bool, uint256) {
        if (b == 0) return (false, 0);
        return (true, a % b);
    }

    /**
     * @dev Returns the addition of two unsigned integers, reverting on
     * overflow.
     *
     * Counterpart to Solidity's `+` operator.
     *
     * Requirements:
     *
     * - Addition cannot overflow.
     */
    function add(uint256 a, uint256 b) internal pure returns (uint256) {
        uint256 c = a + b;
        require(c >= a, "SafeMath: addition overflow");
        return c;
    }

    /**
     * @dev Returns the subtraction of two unsigned integers, reverting on
     * overflow (when the result is negative).
     *
     * Counterpart to Solidity's `-` operator.
     *
     * Requirements:
     *
     * - Subtraction cannot overflow.
     */
    function sub(uint256 a, uint256 b) internal pure returns (uint256) {
        require(b <= a, "SafeMath: subtraction overflow");
        return a - b;
    }

    /**
     * @dev Returns the multiplication of two unsigned integers, reverting on
     * overflow.
     *
     * Counterpart to Solidity's `*` operator.
     *
     * Requirements:
     *
     * - Multiplication cannot overflow.
     */
    function mul(uint256 a, uint256 b) internal pure returns (uint256) {
        if (a == 0) return 0;
        uint256 c = a * b;
        require(c / a == b, "SafeMath: multiplication overflow");
        return c;
    }

    /**
     * @dev Returns the integer division of two unsigned integers, reverting on
     * division by zero. The result is rounded towards zero.
     *
     * Counterpart to Solidity's `/` operator. Note: this function uses a
     * `revert` opcode (which leaves remaining gas untouched) while Solidity
     * uses an invalid opcode to revert (consuming all remaining gas).
     *
     * Requirements:
     *
     * - The divisor cannot be zero.
     */
    function div(uint256 a, uint256 b) internal pure returns (uint256) {
        require(b > 0, "SafeMath: division by zero");
        return a / b;
    }

    /**
     * @dev Returns the remainder of dividing two unsigned integers. (unsigned integer modulo),
     * reverting when dividing by zero.
     *
     * Counterpart to Solidity's `%` operator. This function uses a `revert`
     * opcode (which leaves remaining gas untouched) while Solidity uses an
     * invalid opcode to revert (consuming all remaining gas).
     *
     * Requirements:
     *
     * - The divisor cannot be zero.
     */
    function mod(uint256 a, uint256 b) internal pure returns (uint256) {
        require(b > 0, "SafeMath: modulo by zero");
        return a % b;
    }

    /**
     * @dev Returns the subtraction of two unsigned integers, reverting with custom message on
     * overflow (when the result is negative).
     *
     * CAUTION: This function is deprecated because it requires allocating memory for the error
     * message unnecessarily. For custom revert reasons use {trySub}.
     *
     * Counterpart to Solidity's `-` operator.
     *
     * Requirements:
     *
     * - Subtraction cannot overflow.
     */
    function sub(uint256 a, uint256 b, string memory errorMessage) internal pure returns (uint256) {
        require(b <= a, errorMessage);
        return a - b;
    }

    /**
     * @dev Returns the integer division of two unsigned integers, reverting with custom message on
     * division by zero. The result is rounded towards zero.
     *
     * CAUTION: This function is deprecated because it requires allocating memory for the error
     * message unnecessarily. For custom revert reasons use {tryDiv}.
     *
     * Counterpart to Solidity's `/` operator. Note: this function uses a
     * `revert` opcode (which leaves remaining gas untouched) while Solidity
     * uses an invalid opcode to revert (consuming all remaining gas).
     *
     * Requirements:
     *
     * - The divisor cannot be zero.
     */
    function div(uint256 a, uint256 b, string memory errorMessage) internal pure returns (uint256) {
        require(b > 0, errorMessage);
        return a / b;
    }

    /**
     * @dev Returns the remainder of dividing two unsigned integers. (unsigned integer modulo),
     * reverting with custom message when dividing by zero.
     *
     * CAUTION: This function is deprecated because it requires allocating memory for the error
     * message unnecessarily. For custom revert reasons use {tryMod}.
     *
     * Counterpart to Solidity's `%` operator. This function uses a `revert`
     * opcode (which leaves remaining gas untouched) while Solidity uses an
     * invalid opcode to revert (consuming all remaining gas).
     *
     * Requirements:
     *
     * - The divisor cannot be zero.
     */
    function mod(uint256 a, uint256 b, string memory errorMessage) internal pure returns (uint256) {
        require(b > 0, errorMessage);
        return a % b;
    }
}

/**
 * @dev Collection of functions related to the address type
 */
library AddressUpgradeable {
    /**
     * @dev Returns true if `account` is a contract.
     *
     * [IMPORTANT]
     * ====
     * It is unsafe to assume that an address for which this function returns
     * false is an externally-owned account (EOA) and not a contract.
     *
     * Among others, `isContract` will return false for the following
     * types of addresses:
     *
     *  - an externally-owned account
     *  - a contract in construction
     *  - an address where a contract will be created
     *  - an address where a contract lived, but was destroyed
     * ====
     */
    function isContract(address account) internal view returns (bool) {
        // This method relies on extcodesize, which returns 0 for contracts in
        // construction, since the code is only stored at the end of the
        // constructor execution.

        uint256 size;
        // solhint-disable-next-line no-inline-assembly
        assembly { size := extcodesize(account) }
        return size > 0;
    }

    /**
     * @dev Replacement for Solidity's `transfer`: sends `amount` wei to
     * `recipient`, forwarding all available gas and reverting on errors.
     *
     * https://eips.ethereum.org/EIPS/eip-1884[EIP1884] increases the gas cost
     * of certain opcodes, possibly making contracts go over the 2300 gas limit
     * imposed by `transfer`, making them unable to receive funds via
     * `transfer`. {sendValue} removes this limitation.
     *
     * https://diligence.consensys.net/posts/2019/09/stop-using-soliditys-transfer-now/[Learn more].
     *
     * IMPORTANT: because control is transferred to `recipient`, care must be
     * taken to not create reentrancy vulnerabilities. Consider using
     * {ReentrancyGuard} or the
     * https://solidity.readthedocs.io/en/v0.5.11/security-considerations.html#use-the-checks-effects-interactions-pattern[checks-effects-interactions pattern].
     */
    function sendValue(address payable recipient, uint256 amount) internal {
        require(address(this).balance >= amount, "Address: insufficient balance");

        // solhint-disable-next-line avoid-low-level-calls, avoid-call-value
        (bool success, ) = recipient.call{ value: amount }("");
        require(success, "Address: unable to send value, recipient may have reverted");
    }

    /**
     * @dev Performs a Solidity function call using a low level `call`. A
     * plain`call` is an unsafe replacement for a function call: use this
     * function instead.
     *
     * If `target` reverts with a revert reason, it is bubbled up by this
     * function (like regular Solidity function calls).
     *
     * Returns the raw returned data. To convert to the expected return value,
     * use https://solidity.readthedocs.io/en/latest/units-and-global-variables.html?highlight=abi.decode#abi-encoding-and-decoding-functions[`abi.decode`].
     *
     * Requirements:
     *
     * - `target` must be a contract.
     * - calling `target` with `data` must not revert.
     *
     * _Available since v3.1._
     */
    function functionCall(address target, bytes memory data) internal returns (bytes memory) {
        return functionCall(target, data, "Address: low-level call failed");
    }

    /**
     * @dev Same as {xref-Address-functionCall-address-bytes-}[`functionCall`], but with
     * `errorMessage` as a fallback revert reason when `target` reverts.
     *
     * _Available since v3.1._
     */
    function functionCall(address target, bytes memory data, string memory errorMessage) internal returns (bytes memory) {
        return functionCallWithValue(target, data, 0, errorMessage);
    }

    /**
     * @dev Same as {xref-Address-functionCall-address-bytes-}[`functionCall`],
     * but also transferring `value` wei to `target`.
     *
     * Requirements:
     *
     * - the calling contract must have an ETH balance of at least `value`.
     * - the called Solidity function must be `payable`.
     *
     * _Available since v3.1._
     */
    function functionCallWithValue(address target, bytes memory data, uint256 value) internal returns (bytes memory) {
        return functionCallWithValue(target, data, value, "Address: low-level call with value failed");
    }

    /**
     * @dev Same as {xref-Address-functionCallWithValue-address-bytes-uint256-}[`functionCallWithValue`], but
     * with `errorMessage` as a fallback revert reason when `target` reverts.
     *
     * _Available since v3.1._
     */
    function functionCallWithValue(address target, bytes memory data, uint256 value, string memory errorMessage) internal returns (bytes memory) {
        require(address(this).balance >= value, "Address: insufficient balance for call");
        require(isContract(target), "Address: call to non-contract");

        // solhint-disable-next-line avoid-low-level-calls
        (bool success, bytes memory returndata) = target.call{ value: value }(data);
        return _verifyCallResult(success, returndata, errorMessage);
    }

    /**
     * @dev Same as {xref-Address-functionCall-address-bytes-}[`functionCall`],
     * but performing a static call.
     *
     * _Available since v3.3._
     */
    function functionStaticCall(address target, bytes memory data) internal view returns (bytes memory) {
        return functionStaticCall(target, data, "Address: low-level static call failed");
    }

    /**
     * @dev Same as {xref-Address-functionCall-address-bytes-string-}[`functionCall`],
     * but performing a static call.
     *
     * _Available since v3.3._
     */
    function functionStaticCall(address target, bytes memory data, string memory errorMessage) internal view returns (bytes memory) {
        require(isContract(target), "Address: static call to non-contract");

        // solhint-disable-next-line avoid-low-level-calls
        (bool success, bytes memory returndata) = target.staticcall(data);
        return _verifyCallResult(success, returndata, errorMessage);
    }

    function _verifyCallResult(bool success, bytes memory returndata, string memory errorMessage) private pure returns(bytes memory) {
        if (success) {
            return returndata;
        } else {
            // Look for revert reason and bubble it up if present
            if (returndata.length > 0) {
                // The easiest way to bubble the revert reason is using memory via assembly

                // solhint-disable-next-line no-inline-assembly
                assembly {
                    let returndata_size := mload(returndata)
                    revert(add(32, returndata), returndata_size)
                }
            } else {
                revert(errorMessage);
            }
        }
    }
}

/**
 * @dev This is a base contract to aid in writing upgradeable contracts, or any kind of contract that will be deployed
 * behind a proxy. Since a proxied contract can't have a constructor, it's common to move constructor logic to an
 * external initializer function, usually called `initialize`. It then becomes necessary to protect this initializer
 * function so it can only be called once. The {initializer} modifier provided by this contract will have this effect.
 *
 * TIP: To avoid leaving the proxy in an uninitialized state, the initializer function should be called as early as
 * possible by providing the encoded function call as the `_data` argument to {ERC1967Proxy-constructor}.
 *
 * CAUTION: When used with inheritance, manual care must be taken to not invoke a parent initializer twice, or to ensure
 * that all initializers are idempotent. This is not verified automatically as constructors are by Solidity.
 */
abstract contract Initializable {
    /**
     * @dev Indicates that the contract has been initialized.
     */
    bool private _initialized;

    /**
     * @dev Indicates that the contract is in the process of being initialized.
     */
    bool private _initializing;

    /**
     * @dev Modifier to protect an initializer function from being invoked twice.
     */
    modifier initializer() {
        require(_initializing || !_initialized, "Initializable: contract is already initialized");

        bool isTopLevelCall = !_initializing;
        if (isTopLevelCall) {
            _initializing = true;
            _initialized = true;
        }

        _;

        if (isTopLevelCall) {
            _initializing = false;
        }
    }
}

/**
 * @title Interface for contracts conforming to ERC-20
 */
interface ERC20Interface {
    function transferFrom(address from, address to, uint tokens) external returns (bool success);
}

/**
 * @title Interface for contracts conforming to ERC-721
 */
interface ERC721Interface {
    function ownerOf(uint256 _tokenId) external view returns (address _owner);
    function approve(address _to, uint256 _tokenId) external;
    function getApproved(uint256 _tokenId) external view returns (address);
    function isApprovedForAll(address _owner, address _operator) external view returns (bool);
    function safeTransferFrom(address _from, address _to, uint256 _tokenId) external;
    function supportsInterface(bytes4) external view returns (bool);
}

interface ERC721Verifiable is ERC721Interface {
    function verifyFingerprint(uint256, bytes memory) external view returns (bool);
}

contract ContextMixin {
    function _msgSender()
    internal
    view
    returns (address sender)
    {
        if (msg.sender == address(this)) {
            bytes memory array = msg.data;
            uint256 index = msg.data.length;
            assembly {
            // Load the 32 bytes word from memory with the address on the lower 20 bytes, and mask those.
                sender := and(
                mload(add(array, index)),
                0xffffffffffffffffffffffffffffffffffffffff
                )
            }
        } else {
            sender = msg.sender;
        }
        return sender;
    }
}

/**
 * @dev Contract module which provides a basic access control mechanism, where
 * there is an account (an owner) that can be granted exclusive access to
 * specific functions.
 *
 * By default, the owner account will be the one that deploys the contract. This
 * can later be changed with {transferOwnership}.
 *
 * This module is used through inheritance. It will make available the modifier
 * `onlyOwner`, which can be applied to your functions to restrict their use to
 * the owner.
 */
abstract contract Ownable is Initializable, ContextMixin {
    address private _owner;

    event OwnershipTransferred(address indexed previousOwner, address indexed newOwner);

    /**
     * @dev Initializes the contract setting the deployer as the initial owner.
     */
    function Ownable_init() internal initializer {
        address msgSender = _msgSender();
        _owner = msgSender;
        emit OwnershipTransferred(address(0), msgSender);
    }

    /**
     * @dev Returns the address of the current owner.
     */
    function owner() public view virtual returns (address) {
        return _owner;
    }

    /**
     * @dev Throws if called by any account other than the owner.
     */
    modifier onlyOwner() {
        require(owner() == _msgSender(), "Ownable: caller is not the owner");
        _;
    }

    /**
     * @dev Leaves the contract without owner. It will not be possible to call
     * `onlyOwner` functions anymore. Can only be called by the current owner.
     *
     * NOTE: Renouncing ownership will leave the contract without an owner,
     * thereby removing any functionality that is only available to the owner.
     */
    function renounceOwnership() public virtual onlyOwner {
        emit OwnershipTransferred(_owner, address(0));
        _owner = address(0);
    }

    /**
     * @dev Transfers ownership of the contract to a new account (`newOwner`).
     * Can only be called by the current owner.
     */
    function transferOwnership(address newOwner) public virtual onlyOwner {
        require(newOwner != address(0), "Ownable: new owner is the zero address");
        emit OwnershipTransferred(_owner, newOwner);
        _owner = newOwner;
    }
}

/**
 * @dev Contract module which allows children to implement an emergency stop
 * mechanism that can be triggered by an authorized account.
 *
 * This module is used through inheritance. It will make available the
 * modifiers `whenNotPaused` and `whenPaused`, which can be applied to
 * the functions of your contract. Note that they will not be pausable by
 * simply including this module, only once the modifiers are put in place.
 */
abstract contract Pausable is Initializable, ContextMixin {
    /**
     * @dev Emitted when the pause is triggered by `account`.
     */
    event Paused(address account);

    /**
     * @dev Emitted when the pause is lifted by `account`.
     */
    event Unpaused(address account);

    bool private _paused;

    /**
     * @dev Initializes the contract in unpaused state.
     */
    function Pausable_init() internal initializer {
        _paused = false;
    }

    /**
     * @dev Returns true if the contract is paused, and false otherwise.
     */
    function paused() public view virtual returns (bool) {
        return _paused;
    }

    /**
     * @dev Modifier to make a function callable only when the contract is not paused.
     *
     * Requirements:
     *
     * - The contract must not be paused.
     */
    modifier whenNotPaused() {
        require(!paused(), "Pausable: paused");
        _;
    }

    /**
     * @dev Modifier to make a function callable only when the contract is paused.
     *
     * Requirements:
     *
     * - The contract must be paused.
     */
    modifier whenPaused() {
        require(paused(), "Pausable: not paused");
        _;
    }

    /**
     * @dev Triggers stopped state.
     *
     * Requirements:
     *
     * - The contract must not be paused.
     */
    function _pause() internal virtual whenNotPaused {
        _paused = true;
        emit Paused(_msgSender());
    }

    /**
     * @dev Returns to normal state.
     *
     * Requirements:
     *
     * - The contract must be paused.
     */
    function _unpause() internal virtual whenPaused {
        _paused = false;
        emit Unpaused(_msgSender());
    }
}

contract EIP712Base {
    struct EIP712Domain {
        string name;
        string version;
        address verifyingContract;
        bytes32 salt;
    }

    bytes32 internal constant EIP712_DOMAIN_TYPEHASH = keccak256(
        bytes(
            "EIP712Domain(string name,string version,address verifyingContract,bytes32 salt)"
        )
    );
    bytes32 public domainSeparator;

    // supposed to be called once while initializing.
    // one of the contractsa that inherits this contract follows proxy pattern
    // so it is not possible to do this in a constructor
    function _initializeEIP712(
        string memory name,
        string memory version
    )
    internal
    {
        domainSeparator = keccak256(
            abi.encode(
                EIP712_DOMAIN_TYPEHASH,
                keccak256(bytes(name)),
                keccak256(bytes(version)),
                address(this),
                bytes32(getChainId())
            )
        );
    }

    function getChainId() public pure returns (uint256) {
        uint256 id;
        assembly {
            id := chainid()
        }
        return id;
    }

    /**
     * Accept message hash and returns hash message in EIP712 compatible form
     * So that it can be used to recover signer from signature signed using EIP712 formatted data
     * https://eips.ethereum.org/EIPS/eip-712
     * "\\x19" makes the encoding deterministic
     * "\\x01" is the version byte to make it compatible to EIP-191
     */
    function toTypedMessageHash(bytes32 messageHash)
    internal
    view
    returns (bytes32)
    {
        return
        keccak256(
            abi.encodePacked("\x19\x01", domainSeparator, messageHash)
        );
    }
}

contract NativeMetaTransaction is EIP712Base {
    bytes32 private constant META_TRANSACTION_TYPEHASH = keccak256(
        bytes(
            "MetaTransaction(uint256 nonce,address from,bytes functionSignature)"
        )
    );
    event MetaTransactionExecuted(
        address userAddress,
        address relayerAddress,
        bytes functionSignature
    );
    mapping(address => uint256) nonces;

    /*
     * Meta transaction structure.
     * No point of including value field here as if user is doing value transfer then he has the funds to pay for gas
     * He should call the desired function directly in that case.
     */
    struct MetaTransaction {
        uint256 nonce;
        address from;
        bytes functionSignature;
    }

    function executeMetaTransaction(
        address userAddress,
        bytes memory functionSignature,
        bytes32 sigR,
        bytes32 sigS,
        uint8 sigV
    ) public payable returns (bytes memory) {
        MetaTransaction memory metaTx = MetaTransaction({
        nonce: nonces[userAddress],
        from: userAddress,
        functionSignature: functionSignature
        });

        require(
            verify(userAddress, metaTx, sigR, sigS, sigV),
            "NMT#executeMetaTransaction: SIGNER_AND_SIGNATURE_DO_NOT_MATCH"
        );

        // increase nonce for user (to avoid re-use)
        nonces[userAddress] = nonces[userAddress] + 1;

        emit MetaTransactionExecuted(
            userAddress,
            msg.sender,
            functionSignature
        );

        // Append userAddress and relayer address at the end to extract it from calling context
        (bool success, bytes memory returnData) = address(this).call(
            abi.encodePacked(functionSignature, userAddress)
        );
        require(success, "NMT#executeMetaTransaction: CALL_FAILED");

        return returnData;
    }

    function hashMetaTransaction(MetaTransaction memory metaTx)
    internal
    pure
    returns (bytes32)
    {
        return
        keccak256(
            abi.encode(
                META_TRANSACTION_TYPEHASH,
                metaTx.nonce,
                metaTx.from,
                keccak256(metaTx.functionSignature)
            )
        );
    }

    function getNonce(address user) public view returns (uint256 nonce) {
        nonce = nonces[user];
    }

    function verify(
        address signer,
        MetaTransaction memory metaTx,
        bytes32 sigR,
        bytes32 sigS,
        uint8 sigV
    ) internal view returns (bool) {
        require(signer != address(0), "NMT#verify: INVALID_SIGNER");
        return
        signer ==
        ecrecover(
            toTypedMessageHash(hashMetaTransaction(metaTx)),
            sigV,
            sigR,
            sigS
        );
    }
}
contract MarketplaceStorage {
    address public teamAddress;
    ERC20Interface public hccToken;

    struct Order {
        //Order type 10 normal \ 20 auction
        uint256 tpe;
        // Order ID
        bytes32 id;
        // Owner of the NFT
        address seller;
        // Price (in wei) for the published item
        uint256 price;

        // Latest Bidder
        address bidder;
        // Latest Bid
        uint256 bid;

        // Time when this sale ends
        uint256 expiresAt;
    }

    // From ERC721 registry assetId to Order (to avoid asset collision)
    mapping(uint256 => Order) public orderMap;

    uint256 public tradeFee;
    uint256 public publicationFeeInWei;
    // The bid order extension time of bid success (in second)
    uint256 public extensionTime;
    // At the minimum limit that require extension time after bid success (in second)
    uint256 public extensionTimeLimit;
    // At the minimum limit that bid
    uint256 public bidLimit;

    bytes4 public constant InterfaceId_ValidateFingerprint = bytes4(
        keccak256("verifyFingerprint(uint256,bytes)")
    );

    bytes4 public constant ERC721_Interface = bytes4(0x80ac58cd);

    //Order type 10 normal \ 20 auction
    uint256 public constant ORDER_TYPE_NORMAL = 10;
    uint256 public constant ORDER_TYPE_AUCTION = 20;

    address public nftAddress;

    // EVENTS
    event OrderCreated(
        uint256 tpe,
        bytes32 id,
        uint256 indexed assetId,
        address indexed seller,
        uint256 price,
        uint256 expiresAt
    );
    event OrderSuccessful(
        bytes32 id,
        uint256 indexed assetId,
        address indexed seller,
        uint256 price,
        address indexed buyer,
        uint256 timestamp
    );
    event OrderCancelled(
        bytes32 id,
        uint256 indexed assetId,
        address indexed seller
    );
    event OrderBidSuccess(
        bytes32 id,
        uint256 indexed assetId,
        address indexed seller,
        address indexed bidder,
        uint256 latestPrice,
        uint256 expiresAt,
        uint256 timestamp
    );
    event OrderBidFailed(
        bytes32 id,
        address indexed bidder,
        uint256 latestPrice
    );

    event ChangedTeamAddress(address teamAddress);
    event ChangedPublicationFee(uint256 publicationFee);
    event ChangedTradeFee(uint256 tradeFee);
    event ChangedBidLimit(uint256 bidLimit);
    event ChangedExtensionTimeAndLimit(uint256 extensionTime,uint256 extensionTimeLimit);
}


contract HCCMarketplace is Initializable, Ownable, Pausable, MarketplaceStorage, NativeMetaTransaction {
    using SafeMathUpgradeable for uint256;
    using AddressUpgradeable for address;

    /**
      * @dev Initialize this contract. Acts as a constructor
    * @param _hccToken - Address of the main ERC20 accepted for this marketplace
    */
    constructor (
        address _hccToken,
        address _owner,
        address _teamAddress,
        address _nftAddress
    )
    public
    initializer
    {
        Ownable_init();
        Pausable_init();

        // EIP712 init
        _initializeEIP712('TKS Marketplace', '1');

        // Fee init
        tradeFee = 0;
        publicationFeeInWei = 0;

        // extension time init
        extensionTime = 0;
        extensionTimeLimit = 0;

        setTeamAddress(_teamAddress);

        require(_owner != address(0), "Invalid owner");
        transferOwnership(_owner);

        require(_hccToken != address(0), "Invalid address");
        hccToken = ERC20Interface(_hccToken);
        require(_nftAddress != address(0), "Invalid nftAddress");
        nftAddress = _nftAddress;
    }

    /**
      * @dev Sets the publication fee that's charged to users to publish items
    * @param _teamAddress - address of the teamAddress for collecting fees
    */
    function setTeamAddress(address _teamAddress) public onlyOwner {
        require(_teamAddress != address(0), "Invalid teamAddress");
        teamAddress = _teamAddress;
        emit ChangedTeamAddress(teamAddress);
    }

    /**
      * @dev Sets the publication fee that's charged to users to publish items
    * @param _publicationFee - Fee amount in wei this contract charges to publish an item
    */
    function setPublicationFee(uint256 _publicationFee) external onlyOwner {
        require(_publicationFee < 10000, "The owner cut should be between 0 and 10000");
        publicationFeeInWei = _publicationFee;
        emit ChangedPublicationFee(publicationFeeInWei);
    }

    /**
      * @dev Sets the share cut for the owner of the contract that's
    *  charged to the seller on a successful sale
    * @param _tradeFee - Share amount, from 0 to 999,999
    */
    function setTradeFee(uint256 _tradeFee) public onlyOwner {
        require(_tradeFee < 10000, "The owner cut should be between 0 and 10000");

        tradeFee = _tradeFee;
        emit ChangedTradeFee(tradeFee);
    }

    /**
      * @dev Set the bid order extension time and minimum limit
    * @param _extensionTime - The bid order extension time of bid success (in second)
    * @param _extensionTimeLimit - At the minimum limit that require extension time after bid success (in second)
    */
    function setExtensionTimeAndLimit(uint256 _extensionTime, uint256 _extensionTimeLimit) public onlyOwner {
        require(extensionTime == 0 && extensionTimeLimit == 0, "If extension time equal zero, limit must be zero!");
        extensionTime = _extensionTime;
        extensionTimeLimit = _extensionTimeLimit;
        emit ChangedExtensionTimeAndLimit(_extensionTime, _extensionTimeLimit);
    }

    /**
      * @dev Set the minimum limit that bid
    * @param _bidLimit - The bid order bid limit
    */
    function setBidLimit(uint256 _bidLimit) public onlyOwner {
        bidLimit = _bidLimit;
        emit ChangedBidLimit(_bidLimit);
    }

    /**
      * @dev Creates a new normal order
    * @param assetId - ID of the published NFT
    * @param priceInWei - Price in Wei for the supported coin
    */
    function createNormalOrder(
        uint256 assetId,
        uint256 priceInWei
    )
    public
    whenNotPaused
    {
        _createOrder(
            ORDER_TYPE_NORMAL,
            assetId,
            priceInWei,
            0
        );
    }

    /**
      * @dev Creates a new auction order
    * @param assetId - ID of the published NFT
    * @param priceInWei - Price in Wei for the supported coin
    * @param expiresAt - Duration of the order (in seconds)
    */
    function createAuctionOrder(
        uint256 assetId,
        uint256 priceInWei,
        uint256 expiresAt
    )
    public
    whenNotPaused
    {
        _createOrder(
            ORDER_TYPE_AUCTION,
            assetId,
            priceInWei,
            expiresAt
        );
    }

    /**
      * @dev Cancel an already published order
    *  can only be canceled by seller or the contract owner
    * @param assetId - ID of the published NFT
    */
    function cancelOrder(uint256 assetId) public whenNotPaused {
        _cancelOrder(assetId);
    }

    /**
      * @dev Executes the sale for a published NFT and checks for the asset fingerprint
    * @param assetId - ID of the published NFT
    * @param price - transaction price of NFT
    * @param fingerprint - Verification info for the asset
    */
    function safeExecuteOrder(
        uint256 assetId,
        uint256 price,
        bytes memory fingerprint
    )
    public
    whenNotPaused
    {
        _executeOrder(
            assetId,
            price,
            fingerprint
        );
    }

    /**
      * @dev Executes the sale for a published NFT
    * @param assetId - ID of the published NFT
    * @param price - transaction price of NFT
    */
    function executeOrder(
        uint256 assetId,
        uint256 price
    )
    public
    whenNotPaused
    {
        _executeOrder(
            assetId,
            price,
            ""
        );
    }

    /**
      * @dev bid an auction order
    * @param assetId - ID of the published NFT
    * @param bid - bid price
    */
    function bidAuctionOrder(uint256 assetId, uint256 bid) public whenNotPaused {
        _bidAuctionOrder(assetId, bid);
    }

    /**
      * @dev Creates a new order
    * @param tpe - type of order
    * @param assetId - ID of the published NFT
    * @param priceInWei - Price in Wei for the supported coin
    * @param expiresAt - Duration of the order (in hours)
    */
    function _createOrder(
        uint256 tpe,
        uint256 assetId,
        uint256 priceInWei,
        uint256 expiresAt
    )
    internal
    {
        address sender = _msgSender();

        ERC721Interface nftRegistry = ERC721Interface(nftAddress);
        address assetOwner = nftRegistry.ownerOf(assetId);

        require(sender == assetOwner, "_createOrder::Only the owner can create orders");
        require(
            nftRegistry.getApproved(assetId) == address(this) || nftRegistry.isApprovedForAll(assetOwner, address(this)),
            "_createOrder::The contract is not authorized to manage the asset"
        );
        require(priceInWei > 0, "Price should be bigger than 0");
        require(tpe==ORDER_TYPE_NORMAL || expiresAt > block.timestamp.add(1 minutes), "_createOrder::Publication auction order should be more than 1 minute in the future");

        Order memory order = orderMap[assetId];
        require(order.id > 0, "The asset has published!");

        bytes32 orderId = keccak256(
            abi.encodePacked(
                block.timestamp,
                assetOwner,
                assetId,
                nftAddress,
                priceInWei
            )
        );

        orderMap[assetId] = Order({
            tpe: tpe,
            id: orderId,
            seller: assetOwner,
            price: priceInWei,
            bidder: address(0),
            bid: 0,
            expiresAt: expiresAt
        });

        // Check if there's a publication fee and
        // transfer the amount to marketplace owner
        if (publicationFeeInWei > 0) {
            require(
                hccToken.transferFrom(sender, teamAddress, publicationFeeInWei),
                "_createOrder::Transfering the publication fee to the Marketplace owner failed"
            );
        }

        // deposit nft to contract
        if(order.tpe == ORDER_TYPE_AUCTION){
            nftRegistry.safeTransferFrom(assetOwner, address(this), assetId);
        }

        emit OrderCreated(
            tpe,
            orderId,
            assetId,
            assetOwner,
            priceInWei,
            expiresAt
        );
    }

    /**
      * @dev Cancel an already published order
    *  can only be canceled by seller or the contract owner
    * @param assetId - ID of the published NFT
    */
    function _cancelOrder(uint256 assetId) internal returns (Order memory) {
        address sender = _msgSender();
        Order memory order = orderMap[assetId];

        require(order.id != 0, "Asset not published");
        require(order.seller == sender || sender == owner(), "Unauthorized user");

        ERC721Interface nftRegistry = ERC721Interface(nftAddress);
        bytes32 orderId = order.id;
        address orderSeller = order.seller;
        address orderBidder = order.bidder;
        uint256 bid = order.bid;

        delete orderMap[assetId];

        // if order type is auction,return bid to bidder
        if(order.tpe == ORDER_TYPE_AUCTION){
            if(orderBidder != address(0)){
                _returnBidder(orderBidder, bid);
            }
            // Return from contract
            nftRegistry.safeTransferFrom(address(this), orderSeller, assetId);
        }


        emit OrderCancelled(
            orderId,
            assetId,
            orderSeller
        );

        return order;
    }

    /**
      * @dev Executes the sale for a published NFT
    * @param assetId - ID of the published NFT
    * @param _price - transaction price of NFT
    * @param fingerprint - Verification info for the asset
    */
    function _executeOrder(
        uint256 assetId,
        uint256 _price,
        bytes memory fingerprint
    )
    internal returns (Order memory)
    {
        address _nftAddress = nftAddress;
        uint256 _assetId = assetId;

        address sender = _msgSender();

        ERC721Verifiable nftRegistry = ERC721Verifiable(_nftAddress);

        if (nftRegistry.supportsInterface(InterfaceId_ValidateFingerprint)) {
            require(
                nftRegistry.verifyFingerprint(_assetId, fingerprint),
                "The asset fingerprint is not valid"
            );
        }
        Order memory order = orderMap[_assetId];
        require(order.id != 0, "Asset not published");

        bytes32 orderId = order.id;
        address seller = order.seller;
        uint256 tpe = order.tpe;

        require(seller != address(0), "Invalid address");
        require(seller != sender, "Unauthorized user");
        require(address(this) == nftRegistry.ownerOf(_assetId), "The asset not deposit!");

        uint256 price = 0;
        uint256 timestamp = block.timestamp;
        if(tpe == ORDER_TYPE_NORMAL){
            price = order.price;
            require(price == _price, "The price is not correct");
            if(seller != nftRegistry.ownerOf(_assetId)){
                delete orderMap[_assetId];
                emit OrderCancelled(
                    orderId,
                    _assetId,
                    seller
                );
                revert( "The seller is no longer the owner");
            }
            require(
                hccToken.transferFrom(sender, address(this) , price),
                "_executeOrder::Transfering the sale amount to the seller failed"
            );
        }else if (tpe == ORDER_TYPE_AUCTION){
            price = order.bid;
            require(price == _price, "The price is not correct");
            require(sender == order.bidder, "The auction order unauthorized user");
            require(timestamp > order.expiresAt, "The auction order not expired");
        }else{
            revert("Error Type!");
        }
        delete orderMap[_assetId];

        // send token from contract to seller
        _transferMoney(price, seller, address(this));

        if(tpe == ORDER_TYPE_NORMAL){
            // Transfer asset owner
            nftRegistry.safeTransferFrom(
                seller,
                sender,
                _assetId
            );
        }else if (tpe == ORDER_TYPE_AUCTION){
            // Transfer asset owner
            nftRegistry.safeTransferFrom(
                address(this),
                sender,
                _assetId
            );
        }else{
            revert("Error Type!");
        }

        emit OrderSuccessful(
            orderId,
            _assetId,
            seller,
            price,
            sender,
            timestamp
        );

        return order;
    }

    /**
      * @dev bid an auction order
    * @param assetId - ID of the published NFT
    * @param bid - Price in Wei for the supported coin
    */
    function _bidAuctionOrder(
        uint256 assetId,
        uint256 bid
    )
    internal
    {
        address _nftAddress = nftAddress;
        uint256 _assetId = assetId;

        address sender = _msgSender();

        ERC721Verifiable nftRegistry = ERC721Verifiable(_nftAddress);

        Order memory order = orderMap[_assetId];
        bytes32 orderId = order.id;

        require(orderId != 0, "Asset not published");

        address seller = order.seller;

        uint256 timestamp = block.timestamp;
        require(address(this) == nftRegistry.ownerOf(_assetId), "The asset not deposit!");
        require(seller != address(0), "Invalid address");
        require(seller != sender, "Unauthorized user");
        require(order.bidder != sender, "Unauthorized user");
        require(timestamp < order.expiresAt, "The order expired");
        require(order.bid < bid, "Bid cannot be lower than last time");

        address oldBidder = order.bidder;
        uint256 oldBid = order.bid;
        order.bidder = sender;
        order.bid = bid;
        if(order.expiresAt - block.timestamp < extensionTimeLimit){
            order.expiresAt = order.expiresAt + extensionTime;
        }
        require(
            hccToken.transferFrom(sender, address(this), bid),
            "_bidAuctionOrder::Transfer of bid failed"
        );
        _returnBidder(oldBidder, oldBid);
        emit OrderBidSuccess(
            orderId,
            _assetId,
            seller,
            sender,
            order.bid,
            order.expiresAt,
            timestamp
        );
    }

    function _transferMoney(
        uint256 price,
        address seller,
        address sender
    ) internal
    {
        uint256 fee = _calculateFee(price);
        // Transfer fee amount for marketplace Owner
        require(
            hccToken.transferFrom(sender, teamAddress, fee),
            "_transferMoney::Transfering the cut to the Marketplace owner failed"
        );

        // Transfer sale amount to seller
        require(
            hccToken.transferFrom(sender, seller, price.sub(fee)),
            "_transferMoney::Transfering the sale amount to the seller failed"
        );
    }

    /**
    * @dev Order Cancellation Return to Bidder
    * @param bidder - address bidder
    * @param bid - latest price
    */
    function _returnBidder(address bidder, uint256 bid) internal {
        require(
            hccToken.transferFrom(address(this), bidder, bid),
            "_returnBidder::return bidder failed"
        );
    }

    /**
      * @dev Calculate fee
    * - internal view function, called inside buy(), exchange() function
    * @param price The price of transaction
    */
    function _calculateFee(uint256 price) internal view returns (uint256 fee) {
        fee = price.mul(tradeFee).div(1000000);
    }

}