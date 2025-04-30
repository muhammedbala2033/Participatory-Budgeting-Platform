# 🗳️ Participatory Budgeting Smart Contract

A decentralized platform for community-driven budget allocation decisions built on Stacks blockchain.

## 🎯 Features

- Create budget proposals
- Vote on existing proposals
- Track proposal status
- Automatic proposal finalization
- Budget constraints enforcement

## 🔧 Technical Details

- Minimum proposal amount: 1000 µSTX
- Total budget cap: 1,000,000 µSTX
- Voting period: 144 blocks (~24 hours)

## 📚 Usage

### Create a Proposal
```clarity
(contract-call? .participatory-budgeting create-proposal "Road Repair" "Fix potholes in downtown area" u50000)
```

### Vote on a Proposal
```clarity
(contract-call? .participatory-budgeting vote u1)
```

### Check Proposal Details
```clarity
(contract-call? .participatory-budgeting get-proposal u1)
```

### Finalize a Proposal
```clarity
(contract-call? .participatory-budgeting finalize-proposal u1)
```

## 🔐 Security

- One vote per address per proposal
- Automatic voting period enforcement
- Budget constraints checked at proposal creation

## 🤝 Contributing

Feel free to submit issues and enhancement requests!